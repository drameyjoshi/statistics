library(sqldf)
library(chron)
library(ggplot2)
trains <- read.csv("train_schedules.csv")

# Part A
query_a1 <-
  "select station_code, train_number from trains where arrival != 'None' and departure != 'None'"
ta <- sqldf(query_a1)
result_a <-
  sqldf(
    "select station_code, count(train_number) as ntrains from ta group by station_code order by 2 desc"
  )

stn_code_with_max_trains <- result_a[1, "station_code"]
stn_name_with_max_trains <-
  trains[trains$station_code == stn_code_with_max_trains, c("station_name")][1]

cat(paste("Station with most trains is", stn_name_with_max_trains))

# Part B
# All trains departing from SBC
tb1 <-
  sqldf(
    "select a.train_number, a.station_code, a.arrival, a.day, a.departure
     from trains a inner join (
      select train_number
      from trains
      where departure != 'None'
      and station_code = 'SBC'
    ) b on b.train_number = a.train_number"
  )

# Some time values will not be converted because they are set to "None". Trains
# start from those stations.
tb1$arrival_time = chron(times = tb1$arrival)
tb1$departure_time = chron(times = tb1$departure)

tb2 <-
  sqldf("select train_number, day, departure_time from tb1 where station_code = 'SBC'")

tb3 <- sqldf(
  "select a.train_number,
             a.station_code,
             a.day as arrival_day,
             a.arrival_time,
             b.day as departure_day,
             b.departure_time
             from tb1 a inner join tb2 b on a.train_number = b.train_number
             where a.day >= b.day or a.arrival_time >= b.departure_time
             order by a.train_number, b.day, b.departure_time, a.day, a.arrival_time"
)

tb4 <- sqldf(
  "select
             train_number,
             station_code,
             arrival_day,
             arrival_time,
             departure_day,
             departure_time,
             case when duration > diff_time then duration
             else diff_time end as duration
             from
             (
                select
                 train_number,
                 station_code,
                 arrival_day,
                 arrival_time,
                 departure_day,
                 departure_time,
                 arrival_time - departure_time as diff_time,
                 arrival_day - departure_day as diff_day,
                 arrival_time + (arrival_day - departure_day) as duration
                 from tb3
             )"
)

tb5 <- sqldf(
  "select
             train_number,
             station_code,
             duration,
             rank() over(order by duration desc) as seq
             from tb4
             group by
             train_number,
             station_code"
)
longest_train_number <- tb5[tb5$seq == 1, ]$train_number
longest_stn_code <- tb5[tb5$seq == 1, ]$station_code
longest_train_name <-
  unique(trains[trains$train_number == longest_train_number, "train_name"])
longest_stn_name <-
  unique(trains[trains$station_code == longest_stn_code, "station_name"])

cat(
  paste(
    "Longest journey is to",
    longest_stn_name,
    "by",
    longest_train_name,
    "number",
    longest_train_number
  )
)

# Part C
all_trains <- unique(trains$train_number)
train_classifier <- data.frame(train_number = all_trains)

classify <- function(train_number) {
  digit_vec <- strsplit(train_number, "")[[1]]
  result <- FALSE
  if (digit_vec[1] == "0" ||
      digit_vec[1] == "1" || digit_vec[1] == "2") {
    if (digit_vec[2] != "2") {
      result <- TRUE
    }
  }
  
  result
}

train_classifier$is_long_distance <-
  apply(train_classifier, MARGIN = 1, FUN = classify)
tb6 <- sqldf(
  "select a.*, c.is_long_distance
             from trains a inner join train_classifier c on
             c.train_number = a.train_number"
)
tb7 <-
  sqldf(
    "select distinct station_name, station_code, train_number
             from tb6
             where is_long_distance = true
             and (arrival = 'None' or departure = 'None')"
  )
tb8 <-
  sqldf(
    "select station_name, station_code, count(train_number) as n_trains
             from tb7
             group by 1, 2
             order by 3 desc"
  )
top20_stn <- sqldf("select * from tb8 limit 20")

final <- sqldf(
  "select
               tb7.station_name,
               tb7.station_code,
               tb7.train_number
               from tb7 inner join top20_stn t20 on t20.station_code = tb7.station_code"
)

extract_2nd_digit <- function(row) {
  digit_vec <- strsplit(row[3], "")[[1]]
  digit_vec[2]
}

final$second_digit <-
  apply(final, MARGIN = 1, FUN = extract_2nd_digit)

for_plotting <- sqldf(
  "select
                      station_code,
                      station_name,
                      second_digit,
                      count(train_number) as n_trains
                      from final
                      group by 1, 2, 3"
)

ggplot(for_plotting[, c("station_name", "second_digit", "n_trains")], aes(x =
                                                                            n_trains)) + geom_histogram() + facet_wrap( ~ station_name)
