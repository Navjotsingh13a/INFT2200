       identification division.
       program-id. A5DEPINC.
       author. Navjot Singh.
       date-written. July 3, 2025.
      * Description: This COBOL program reads department income
      * data and produces a formatted report including totals,
      * averages, highest/lowest locations and departments.

       environment division.
       configuration section.

       input-output section.
       file-control.

           select input-file
               assign to 'INFILE'
               organization is sequential.

           select output-file
               assign to 'OUTFILE'
               organization is sequential.

       data division.
       file section.

      * Input record Format...
       fd input-file
           recording mode is f
           data record is input-line
           record contains 60 characters.

       01 input-line.
           05 il-loc-num                  pic x(5).
           05 il-loc-name                 pic x(20).
           05 il-dept                     pic 9(5)v99 occurs 5 times.

      * Output record Format...
       fd output-file
           recording mode is f
           data record is output-line
           record contains 132 characters.

       01 output-line                    pic x(132).

       working-storage section.

      * Constants...
       77 ws-eof-flag                    pic x value 'N'.
       77 ws-zero-count                  pic 9 value 0.
       77 ws-zero-text                   pic x(4) value 'ZERO'.
       77 ws-loc-counter                 pic 9(4) value 0.
       77 idx                            pic 9 value 1.
       77 ws-total                       pic 9(8)v99 value 0.
       77 ws-avg                         pic 9(8)v99 value 0.

      * Numeric field for Calculation...
       77 ws-calc-num                   pic 9(8)v99.

      * Display fields for output formatting (currency format)...
       01 ws-out-dept.
           05 ws-out-dept-element pic $,$$$,$$$.99 occurs 5 times.

       01 ws-out-total                  pic $,$$$,$$$.99.
       01 ws-out-avg                    pic $,$$$,$$$.99.

      * Totals and tracking...
       01 ws-total-dept.
           05 ws-dept-totals        pic 9(7)v99 occurs 5 times value 0.
           05 ws-grand-total             pic 9(8)v99 value 0.

      * Display totals for output...
       01 ws-dept-total-display.
           05 ws-dept-edit-element pic $,$$$,$$$.99 occurs 5 times.

      * Department names...
       01 ws-dept-names.
          05 ws-dept-name      pic x(10) occurs 5 times.

      * Highest and lowest trackers...
       01 ws-highlow.
           05 ws-high-total              pic 9(8)v99 value 0.
           05 ws-high-loc                pic x(5).
           05 ws-low-total               pic 9(8)v99 value 99999999.
           05 ws-low-loc                 pic x(5).
           05 ws-low-dept-amt            pic 9(7)v99 value 9999999.99.
           05 ws-low-dept-idx            pic 9 value 1.

      * Labels for summary lines...
       01 ws-summary-labels.
           05 ws-totals-label              pic x(8) value "Totals  ".
           05 ws-averages-label            pic x(8) value "Averages".

      * Report titles and headings...
       01 ws-report-heading.
           05 filler                     pic x(87) value spaces.
           05 filler                     pic x(28) value
                      "Navjot Singh, Assignment 5".
           05 filler                     pic x(17) value spaces.

       01 ws-report-title.
           05 filler                     pic x(46) value spaces.
           05 filler                     pic x(40) value
                  "Location Income Volumes by Department".
           05 filler                     pic x(46) value spaces.

      * Column headings...
       01 ws-column-headings.
           05 filler pic x(1) value space.
           05 filler pic x(9) value "Location#".
           05 filler pic x(1) value space.
           05 filler pic x(22) value "Location Name".
           05 filler pic x(2) value spaces.
           05 filler pic x(13) value "Hardware".
           05 filler pic x(2) value spaces.
           05 filler pic x(13) value "Software".
           05 filler pic x(1) value spaces.
           05 filler pic x(10) value "Consulting".
           05 filler pic x(1) value spaces.
           05 filler pic x(9) value "Recycling".
           05 filler pic x(1) value spaces.
           05 filler pic x(9) value "Support".
           05 filler pic x(2) value spaces.
           05 filler pic x(8) value "Total".
           05 filler pic x(6) value spaces.
           05 filler pic x(14) value "Avg".

      * Line format...
       01 ws-detail-line.
           05 ws-dl-loc-num     pic x(8).
           05 ws-dl-loc-name    pic x(22).
           05 ws-dl-hardware    pic x(13).
           05 ws-dl-software    pic x(13).
           05 ws-dl-consulting  pic x(13).
           05 ws-dl-recycling   pic x(13).
           05 ws-dl-support     pic x(13).
           05 ws-dl-total       pic x(13).
           05 ws-dl-avg         pic x(9).

      * Summary line...
       01 ws-summary-line.
           05 ws-sum-label     pic x(8).
           05 filler1          pic x(6) value spaces.
           05 ws-sum-hardware  pic x(13).
           05 ws-sum-software  pic x(13).
           05 ws-sum-consulting pic x(13).
           05 ws-sum-recycling pic x(13).
           05 ws-sum-support   pic x(13).
           05 ws-sum-total     pic x(13).

      * Summary fields...
       01 ws-summary-display.
           05 ws-sum-hardware-dis    pic $,$$$,$$$.99.
           05 ws-sum-software-dis    pic $,$$$,$$$.99.
           05 ws-sum-consulting-dis  pic $,$$$,$$$.99.
           05 ws-sum-recycling-dis   pic $,$$$,$$$.99.
           05 ws-sum-support-dis     pic $,$$$,$$$.99.
           05 ws-sum-total-dis       pic $,$$$,$$$.99.

      * High income line format...
       01 ws-high-income-line.
          05 filler1    pic x(1) value space.
          05 filler2    pic x(1) value space.
          05 high-label pic x(40) value
                "Location with Highest Department Income:".
          05 filler3    pic x(3) value spaces.
          05 high-loc   pic x(5).
          05 filler4    pic x(82) value spaces.

      * Low income line format...
       01 ws-low-income-line.
          05 filler1    pic x(1) value space.
          05 filler2    pic x(1) value space.
          05 low-label  pic x(39) value
                 "Location with Lowest Department Income:".
          05 filler3    pic x(3) value spaces.
          05 low-loc    pic x(5).
          05 filler4    pic x(83) value spaces.

      * Low departmentt line format...
       01 ws-low-dept-line.
          05 filler1    pic x(1) value space.
          05 filler2    pic x(1) value space.
          05 low-dept-label-field pic x(29) value
                 "Departmentwith Lowest Income:".
          05 filler3    pic x(1) value space.
          05 low-dept-name-field pic x(10).
          05 filler4    pic x(90) value spaces.

       procedure division.
       main-line.

           open input input-file
                output output-file

           move "HARDWARE  "  to ws-dept-name(1)
           move "SOFTWARE  "  to ws-dept-name(2)
           move "CONSULTING"  to ws-dept-name(3)
           move "RECYCLING "  to ws-dept-name(4)
           move "SUPPORT   "  to ws-dept-name(5)

           move ws-report-heading to output-line
           write output-line

           move ws-report-title to output-line
           write output-line

           move ws-column-headings to output-line
           write output-line

           perform until ws-eof-flag = 'Y'
               read input-file
                   at end
                       move 'Y' to ws-eof-flag
                   not at end

                       perform process-record
               end-read
           end-perform

           perform write-summary

           close input-file
                 output-file

           stop run.

       process-record.

           move 0 to ws-total
           move 0 to ws-zero-count

           perform varying idx from 1 by 1 until idx > 5
               add il-dept(idx) to ws-total
               if il-dept(idx) > 0
                   add 1 to ws-zero-count
               end-if
               add il-dept(idx) to ws-dept-totals(idx)
               move il-dept(idx) to ws-out-dept-element(idx)

               if il-dept(idx) < ws-low-dept-amt and il-dept(idx) > 0
                   move il-dept(idx) to ws-low-dept-amt
                   move idx to ws-low-dept-idx
               end-if
           end-perform

           if ws-zero-count > 0
               compute ws-avg = ws-total / ws-zero-count
               move ws-avg to ws-out-avg
           else
               move ws-zero-text to ws-dl-avg
           end-if

           add ws-total to ws-grand-total
           add 1 to ws-loc-counter

           if ws-total > ws-high-total
               move ws-total to ws-high-total
               move il-loc-num to ws-high-loc
           end-if

           if ws-total < ws-low-total
               move ws-total to ws-low-total
               move il-loc-num to ws-low-loc
           end-if

      * Move data to detail line fixed fields...
           move il-loc-num to ws-dl-loc-num
           move il-loc-name to ws-dl-loc-name
           move ws-out-dept-element(1) to ws-dl-hardware
           move ws-out-dept-element(2) to ws-dl-software
           move ws-out-dept-element(3) to ws-dl-consulting
           move ws-out-dept-element(4) to ws-dl-recycling
           move ws-out-dept-element(5) to ws-dl-support
           move ws-total to ws-calc-num
           move ws-calc-num to ws-out-total
           move ws-out-total to ws-dl-total

           if ws-zero-count > 0
               move ws-out-avg to ws-dl-avg
           else
               move ws-zero-text to ws-dl-avg
           end-if

           move ws-detail-line to output-line
           write output-line.

       write-summary.

      * Totals line...
           move spaces to ws-summary-line
           move ws-totals-label to ws-sum-label

           perform varying idx from 1 by 1 until idx > 5
               move ws-dept-totals(idx) to ws-calc-num
               move ws-calc-num to ws-dept-edit-element(idx)

               if idx = 1
                   move ws-dept-edit-element(1) to ws-sum-hardware-dis
               end-if

               if idx = 2
                   move ws-dept-edit-element(2) to ws-sum-software-dis
               end-if

               if idx = 3
                   move ws-dept-edit-element(3) to ws-sum-consulting-dis
               end-if

               if idx = 4
                   move ws-dept-edit-element(4) to ws-sum-recycling-dis
               end-if

               if idx = 5
                   move ws-dept-edit-element(5) to ws-sum-support-dis
               end-if
           end-perform

           move ws-grand-total to ws-calc-num
           move ws-calc-num to ws-out-total
           move ws-out-total to ws-sum-total-dis

           move ws-sum-hardware-dis   to ws-sum-hardware
           move ws-sum-software-dis   to ws-sum-software
           move ws-sum-consulting-dis to ws-sum-consulting
           move ws-sum-recycling-dis  to ws-sum-recycling
           move ws-sum-support-dis    to ws-sum-support
           move ws-sum-total-dis      to ws-sum-total

           move ws-summary-line to output-line
           write output-line

      * Averages line...
           move spaces to ws-summary-line
           move ws-averages-label to ws-sum-label

           perform varying idx from 1 by 1 until idx > 5
               if ws-loc-counter > 0
                   compute ws-avg = ws-dept-totals(idx) / ws-loc-counter
                   move ws-avg to ws-calc-num
               else
                   move 0 to ws-calc-num
               end-if

               move ws-calc-num to ws-dept-edit-element(idx)

               if idx = 1
                   move ws-dept-edit-element(1) to ws-sum-hardware-dis
               end-if

               if idx = 2
                   move ws-dept-edit-element(2) to ws-sum-software-dis
               end-if

               if idx = 3
                   move ws-dept-edit-element(3) to ws-sum-consulting-dis
               end-if

               if idx = 4
                   move ws-dept-edit-element(4) to ws-sum-recycling-dis
               end-if

               if idx = 5
                   move ws-dept-edit-element(5) to ws-sum-support-dis
               end-if
           end-perform

           if ws-loc-counter > 0
               compute ws-avg = ws-grand-total / ws-loc-counter
               move ws-avg to ws-calc-num
           else
               move 0 to ws-calc-num
           end-if

           move ws-calc-num to ws-out-avg
           move ws-out-avg to ws-sum-total-dis

           move ws-sum-hardware-dis   to ws-sum-hardware
           move ws-sum-software-dis   to ws-sum-software
           move ws-sum-consulting-dis to ws-sum-consulting
           move ws-sum-recycling-dis  to ws-sum-recycling
           move ws-sum-support-dis    to ws-sum-support
           move ws-sum-total-dis      to ws-sum-total

           move ws-summary-line to output-line
           write output-line

      * Highest/Lowest locations and departments lines...
           move ws-high-loc to high-loc
           move ws-high-income-line to output-line
           write output-line

           move ws-low-loc to low-loc
           move ws-low-income-line to output-line
           write output-line

           move ws-dept-name(ws-low-dept-idx) to low-dept-name-field
           move ws-low-dept-line to output-line
           write output-line.

       end program A5DEPINC.
