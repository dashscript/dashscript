// The most of the rust part of Date is from the time crate.
use std::time::{SystemTime};

const DAYS_OF_MONTH: [[u8; 12]; 2] = [
    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
    [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
];

const DAYS_CUMULATIVE_COMMON_LEAP: [[u16; 12]; 2] = [
    [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334],
    [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335],
];

pub const UNIX_EPOCH_DATE: isize = ymd_into_date(1970, 1, 1);
pub const MS_PER_SECOND: isize = 1000;
pub const MS_PER_MINUTE: isize = 60 * MS_PER_SECOND;
pub const MS_PER_HOUR: isize = 60 * MS_PER_MINUTE;
pub const MS_PER_DAY: isize = 24 * MS_PER_HOUR;

pub const fn leap_year(year: isize) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

pub const fn number_of_days_in_month(month: u8, year: isize) -> Option<u8> {
    if (month >= 1) && (month <= 12) {
        Some(DAYS_OF_MONTH[leap_year(year) as usize][month as usize - 1])
    } else { None }
}

pub const fn ymd_into_date(year: isize, month: u8, day: u8) -> isize {
    (year << 9) | (DAYS_CUMULATIVE_COMMON_LEAP[leap_year(year) as usize][month as usize - 1] + day as u16) as isize
}

pub fn ymd_into_date_checked(year: isize, month: u8, day: u8) -> Option<isize> {
    if (year < -100_000) || (year > 100_000) {
        return None;
    }

    if let Some(days) = number_of_days_in_month(month, year) {
        return if day <= days {
            Some(ymd_into_date(year, month, day))
        } else { None }
    }

    None
}

pub fn date_into_ymd(date: isize) -> (isize, u8, u8) {
    let year = date >> 9;
    let days = DAYS_CUMULATIVE_COMMON_LEAP[leap_year(year) as usize];
    let ordinal = ordinal_from_date(date);
    let (month, day) = if ordinal > days[11] {
        (12, (ordinal - days[11]) as u8)
    } else if ordinal > days[10] {
        (11, (ordinal - days[10]) as u8)
    } else if ordinal > days[9] {
        (10, (ordinal - days[9]) as u8)
    } else if ordinal > days[8] {
        (9, (ordinal - days[8]) as u8)
    } else if ordinal > days[7] {
        (8, (ordinal - days[7]) as u8)
    } else if ordinal > days[6] {
        (7, (ordinal - days[6]) as u8)
    } else if ordinal > days[5] {
        (6, (ordinal - days[5]) as u8)
    } else if ordinal > days[4] {
        (5, (ordinal - days[4]) as u8)
    } else if ordinal > days[3] {
        (4, (ordinal - days[3]) as u8)
    } else if ordinal > days[2] {
        (3, (ordinal - days[2]) as u8)
    } else if ordinal > days[1] {
        (2, (ordinal - days[1]) as u8)
    } else {
        (1, ordinal as u8)
    };

    (year, month, day)
}

pub const fn ordinal_from_date(date: isize) -> u16 {
    (date & 0x1FF) as u16
}

pub fn from_system_time(system_time: SystemTime) -> (isize, isize) {
    let duration = match system_time.duration_since(SystemTime::UNIX_EPOCH) {
        Ok(duration) => duration,
        Err(err) => err.duration(),
    };

    add_duration(duration.as_millis() as isize, UNIX_EPOCH_DATE, 0)
}

pub fn add_duration(millis: isize, date: isize, time: isize) -> (isize, isize) {
    if millis <= 0 {
        return (date, time);
    }

    let added_millis = time + millis;
    let actual_duration_millis = millis + if added_millis < 0 {
        -MS_PER_DAY
    } else if added_millis >= MS_PER_DAY {
        MS_PER_DAY
    } else { 0 };

    (add_date_to_duration(date, actual_duration_millis), added_millis.rem_euclid(MS_PER_DAY))
}

pub fn add_date_to_duration(date: isize, millis: isize) -> isize {
    let (mut year, _, _) = date_into_ymd(date);
    let mut no_days = millis / MS_PER_DAY;

    loop {
        let year_size = if leap_year(year) { 366 } else { 365 };
        if no_days >= year_size {
            no_days -= year_size;
            year += 1;
        } else { break }
    }

    let mut month = 1;
    let days = DAYS_OF_MONTH[leap_year(year) as usize];

    loop {
        let month_size = days[month] as isize;
        if no_days >= month_size {
            no_days -= month_size;
            month += 1;
        } else { break }
    }

    ymd_into_date(year, month as u8, no_days as u8)
}

pub fn date_time_to_ms(date: isize, mut time: isize) -> isize {
    let (year, mut month, date) = date_into_ymd(date);
    let mut current_year = year;
    time += date as isize * MS_PER_DAY;

    if year >= 1970 {
        while current_year != year  {
            let year_size = if leap_year(year) { 366 } else { 365 };
            time += year_size * MS_PER_DAY;
            current_year += 1;
        }

        let days = DAYS_OF_MONTH[leap_year(current_year) as usize];
        while month == 0 {
            month -= 1;
            time += days[month as usize] as isize * MS_PER_DAY;
        }
    } else {
        time = -time;
        while current_year != year {
            let year_size = if leap_year(year) { 366 } else { 365 };
            time -= year_size * MS_PER_DAY;
            current_year -= 1;
        }

        let days = DAYS_OF_MONTH[leap_year(current_year) as usize];
        while month == 0 {
            month -= 1;
            time -= days[month as usize] as isize * MS_PER_DAY;
        }
    }

    time
}