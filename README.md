# hcal

`hcal` is a small calendar program for the terminal. It is intended to be an improvement over the old `cal` program.

## Building

Currently, hcal is just a single file. So building it is easy. Simply run `ghc hcal.hs`. This should create the `hcal` executable.

## Using

hcal can be called using `./hcal [OPTIONS]`. If the executable is located in your search `$PATH`, the `./` is not required. By default, hcal will display the current month. This can be changed by by using the following options:

* `-y[YEAR], --year[=YEAR]` will show the calendar for the whole `YEAR`. If `YEAR` is not specified, the current year will be shown.
* `-r DAY_RANGE, --range=DAY_RANGE` will show the days in the given range. A range is specified using the format `yyyy-mm-dd:yyyy-mm-dd`. Leaving out a date is equivalent to specifying the current date.
* `-c COLUMNS` specifies the number of columns to be used to show the calendar. In other words, it specifies how many months will be displayed on one row.
* `-m, --monday` displays Monday as the first day of the week.
* `-s, --sunday` displays Sunday as the first day of the week.
* `-h, --help` displays a help message.

Note: although hcal is inspired from cal, it has a number of different features that make it incompatible with the original program.

## Sample output

```
$ ./hcal
        2015        
      December      
Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6
 7  8  9 10 11 12 13
14 15 16 17 18 19 20
21 22 23 24 25 26 27
28 29 30 31         

```

```
$ ./hcal -s
        2015        
      December      
Su Mo Tu We Th Fr Sa
       1  2  3  4  5
 6  7  8  9 10 11 12
13 14 15 16 17 18 19
20 21 22 23 24 25 26
27 28 29 30 31      

```

```
$ ./hcal -r 2016-1-5:2016-1-9
        2016        
      January       
Mo Tu We Th Fr Sa Su

    5  6  7  8  9   




```

```
$ ./hcal -y
                                         2015                                         
      January               February               March                 April        
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
          1  2  3  4                     1                     1         1  2  3  4  5
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   2  3  4  5  6  7  8   6  7  8  9 10 11 12
12 13 14 15 16 17 18   9 10 11 12 13 14 15   9 10 11 12 13 14 15  13 14 15 16 17 18 19
19 20 21 22 23 24 25  16 17 18 19 20 21 22  16 17 18 19 20 21 22  20 21 22 23 24 25 26
26 27 28 29 30 31     23 24 25 26 27 28     23 24 25 26 27 28 29  27 28 29 30         
                                            30 31                                     
        May                   June                  July                 August       
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
             1  2  3   1  2  3  4  5  6  7         1  2  3  4  5                  1  2
 4  5  6  7  8  9 10   8  9 10 11 12 13 14   6  7  8  9 10 11 12   3  4  5  6  7  8  9
11 12 13 14 15 16 17  15 16 17 18 19 20 21  13 14 15 16 17 18 19  10 11 12 13 14 15 16
18 19 20 21 22 23 24  22 23 24 25 26 27 28  20 21 22 23 24 25 26  17 18 19 20 21 22 23
25 26 27 28 29 30 31  29 30                 27 28 29 30 31        24 25 26 27 28 29 30
                                                                  31                  
     September              October               November              December      
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
    1  2  3  4  5  6            1  2  3  4                     1      1  2  3  4  5  6
 7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8   7  8  9 10 11 12 13
14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15  14 15 16 17 18 19 20
21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22  21 22 23 24 25 26 27
28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29  28 29 30 31         
                                            30                                        
```

```
$ ./hcal -y2016 -c 3
                              2016                              
      January               February               March        
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
             1  2  3   1  2  3  4  5  6  7      1  2  3  4  5  6
 4  5  6  7  8  9 10   8  9 10 11 12 13 14   7  8  9 10 11 12 13
11 12 13 14 15 16 17  15 16 17 18 19 20 21  14 15 16 17 18 19 20
18 19 20 21 22 23 24  22 23 24 25 26 27 28  21 22 23 24 25 26 27
25 26 27 28 29 30 31  29                    28 29 30 31         

       April                  May                   June        
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
             1  2  3                     1         1  2  3  4  5
 4  5  6  7  8  9 10   2  3  4  5  6  7  8   6  7  8  9 10 11 12
11 12 13 14 15 16 17   9 10 11 12 13 14 15  13 14 15 16 17 18 19
18 19 20 21 22 23 24  16 17 18 19 20 21 22  20 21 22 23 24 25 26
25 26 27 28 29 30     23 24 25 26 27 28 29  27 28 29 30         
                      30 31                                     
        July                 August              September      
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
             1  2  3   1  2  3  4  5  6  7            1  2  3  4
 4  5  6  7  8  9 10   8  9 10 11 12 13 14   5  6  7  8  9 10 11
11 12 13 14 15 16 17  15 16 17 18 19 20 21  12 13 14 15 16 17 18
18 19 20 21 22 23 24  22 23 24 25 26 27 28  19 20 21 22 23 24 25
25 26 27 28 29 30 31  29 30 31              26 27 28 29 30      

      October               November              December      
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                1  2      1  2  3  4  5  6            1  2  3  4
 3  4  5  6  7  8  9   7  8  9 10 11 12 13   5  6  7  8  9 10 11
10 11 12 13 14 15 16  14 15 16 17 18 19 20  12 13 14 15 16 17 18
17 18 19 20 21 22 23  21 22 23 24 25 26 27  19 20 21 22 23 24 25
24 25 26 27 28 29 30  28 29 30              26 27 28 29 30 31   
31                                                              
```

```
$ ./hcal -r 2015-11-15:2016-2-14
                   2015                   
      November              December      
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                          1  2  3  4  5  6
                       7  8  9 10 11 12 13
                  15  14 15 16 17 18 19 20
16 17 18 19 20 21 22  21 22 23 24 25 26 27
23 24 25 26 27 28 29  28 29 30 31         
30                                        

                   2016                   
      January               February      
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
             1  2  3   1  2  3  4  5  6  7
 4  5  6  7  8  9 10   8  9 10 11 12 13 14
11 12 13 14 15 16 17                      
18 19 20 21 22 23 24                      
25 26 27 28 29 30 31                      

```

```
$ ./hcal -h

Usage: hcal [OPTION...]
  -y[YEAR]      --year[=YEAR]      display calendar for whole year; optionally specify the YEAR displayed
  -r DAY_RANGE  --range=DAY_RANGE  display calendar with days in the range specified
  -c COLUMNS                       display calendar with COLUMNS number of columns; COLUMNS must be a positive integer
  -m            --monday           use Monday as first day of the week
  -s            --sunday           use Sunday as first day of the week
  -h            --help             display this help message

See project home page for more information.


```

## License

hcal is distributed under the terms of [The MIT License](http://opensource.org/licenses/MIT).
