       identification division.
       program-id. A3SCOMM.
       date-written. 2025-06-08.
       author. Navjot Singh.
      * Description: This program reads employee sales data,
      * calculates commissions and bonuses, and generates sales
      * commission report...


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

      * Input record 30 characters....
       fd input-file
           recording mode is F
           data record is input-record
           record contains 30 characters.
       01 input-record.
           05 in-emp-no         pic 9(5).
           05 in-emp-name       pic x(9).
           05 in-salary         pic 9(6).
           05 in-comm-rate      pic 9(4).
           05 in-sales          pic 9(6).

       fd output-file
           recording mode is F
           data record is output-line.
       01 output-line              pic x(100).

       working-storage section.

      * Headings....
       01 ws-report-heading.
           05 filler pic x(70) value spaces.
           05 filler pic x(26) value "Navjot Singh, Assignment 3".
           05 filler pic x(04) value spaces.

      * Title...
       01 ws-report-title.
           05 filler pic x(35) value spaces.
           05 filler pic x(25) value "SALES COMMISSION REPORT".
           05 filler pic x(25) value spaces.

      * Column headings...
       01 ws-column-headings.
           05 filler pic x(03) value "No.".
           05 filler pic x(05) value spaces.
           05 filler pic x(10) value "Name".
           05 filler pic x(02) value spaces.
           05 filler pic x(06) value "Rate%".
           05 filler pic x(03) value spaces.
           05 filler pic x(06) value "Sales".
           05 filler pic x(03) value spaces.
           05 filler pic x(06) value "Salary".
           05 filler pic x(03) value spaces.
           05 filler pic x(07) value "Paid".
           05 filler pic x(03) value spaces.
           05 filler pic x(16) value "Bonus / No Bonus".

      * Underline row for the headings....
       01 ws-underline.
           05 filler pic x(03) value "---".
           05 filler pic x(05) value spaces.
           05 filler pic x(10) value "----------".
           05 filler pic x(02) value spaces.
           05 filler pic x(06) value "------".
           05 filler pic x(03) value spaces.
           05 filler pic x(06) value "------".
           05 filler pic x(03) value spaces.
           05 filler pic x(06) value "------".
           05 filler pic x(03) value spaces.
           05 filler pic x(07) value "-------".
           05 filler pic x(03) value spaces.
           05 filler pic x(16) value "----------------".

      * Output detail line fields....
       01 ws-detail-line.
           05 ws-sale-no        pic 9(5).
           05 filler            pic x(05) value spaces.
           05 ws-name           pic x(10).
           05 filler            pic x(02) value spaces.
           05 ws-sales-rate1    pic 99.
           05 filler            pic x(01) value '.'.
           05 ws-sales-rate2    pic 99.
           05 filler            pic x(01) value '%'.
           05 filler            pic x(03) value spaces.
           05 ws-sales1         pic 9(6).
           05 filler            pic x(03) value spaces.
           05 ws-salary         pic 9(6).
           05 filler            pic x(03) value spaces.
           05 ws-paid           pic 9(7).
           05 filler            pic x(03) value spaces.
           05 ws-bonus          pic x(16).

      * Totals and counters....
       01 ws-summary.
           05 ws-total-sales      pic 9(9) value 0.
           05 ws-total-paid       pic 9(9) value 0.
           05 ws-count-employees  pic 9(4) value 0.
           05 ws-count-bonus      pic 9(4) value 0.
           05 ws-percent-bonus    pic 99v99 value 0.

      * Summary fields for printing...
       01 ws-edited-fields.
           05 ws-sales-edited    pic 9(9).
           05 ws-paid-edited     pic 9(9).
           05 ws-count-edited    pic 9(4).
           05 ws-percent-bonus-edited pic zz.99.
           05 ws-earned          pic 9(9).

      * Constants.....
       77 ws-spaceline pic x(99) value spaces.
       77 ws-one-const pic 9 value 1.
       77 ws-lines-per-page-const pic 99 value 10.
       77 ws-eof-flag pic x value 'N'.
       77 ws-const-y pic x value 'Y'.

       77 ws-line-count pic 99 value 0.
       77 ws-page-count pic 99 value 0.

       77 ws-earned-calc pic 9(9) value 0.
       77 ws-paid-calc pic 9(9) value 0.
       77 ws-sales-rate-int pic 9(4) value 0.
       77 ws-rate1-int pic 99 value 0.
       77 ws-rate2-int pic 99 value 0.

       77 ws-bonus-amt pic 9(9) value 0.

       procedure division.
       000-main.
           perform 100-open-files
           perform 200-initialize-output
           perform 300-paging-process
               until ws-eof-flag = ws-const-y
           perform 390-calc-percentages
           perform 400-summary
           perform 500-close-files
           stop run.

      * Open input and output files...
       100-open-files.
           open input input-file
           open output output-file.

      * Processing...
       200-initialize-output.
           write output-line from ws-spaceline
           read input-file
               at end move ws-const-y to ws-eof-flag.

       300-paging-process.
            perform until ws-eof-flag = ws-const-y
               move 0 to ws-line-count
               perform 310-headings

               perform until ws-eof-flag = ws-const-y
               or ws-line-count >= ws-lines-per-page-const
                  perform 320-calc-commission
               end-perform
            end-perform.

      * Print heading and page title...
       310-headings.
           add ws-one-const to ws-page-count
           if ws-page-count = ws-one-const
               write output-line from ws-report-heading
               write output-line from ws-spaceline
               write output-line from ws-report-title
           else
               write output-line from ws-spaceline after advancing page
               write output-line from ws-report-title
           end-if
           write output-line from ws-spaceline
           write output-line from ws-column-headings
           write output-line from ws-underline.

      * Calculate commission, bonus, and total paid...
       320-calc-commission.
           read input-file
               at end move ws-const-y to ws-eof-flag
           end-read

           if ws-eof-flag not = ws-const-y
      * Move values to detail fields...
               move in-emp-name to ws-name
               move in-emp-no to ws-sale-no
               move in-sales to ws-sales1
               move in-salary to ws-salary

               move in-comm-rate to ws-sales-rate-int
               divide ws-sales-rate-int by 100
                   giving ws-rate1-int remainder ws-rate2-int

      * Calculate commission with rounding
               compute ws-earned-calc rounded = (in-sales *
                                               ws-sales-rate-int) / 100

      * Decide bonus...
               move 0 to ws-bonus-amt
               if in-sales >= 100000
                   move 10000 to ws-bonus-amt
                   move "BONUS 10000" to ws-bonus
                   add 1 to ws-count-bonus
               else if in-sales > 50000
                   move 7500 to ws-bonus-amt
                   move "BONUS 7500" to ws-bonus
                   add 1 to ws-count-bonus
               else if in-sales >= 25000
                   move 5000 to ws-bonus-amt
                   move "BONUS 5000" to ws-bonus
                   add 1 to ws-count-bonus
               else
                   move 0 to ws-bonus-amt
                   move "NO BONUS" to ws-bonus
               end-if

      * Calculate total paid...
               compute ws-paid-calc rounded = in-salary +
                                           ws-earned-calc + ws-bonus-amt

               add in-sales to ws-total-sales
               add ws-paid-calc to ws-total-paid
               add 1 to ws-count-employees

               move ws-earned-calc to ws-earned
               move ws-paid-calc to ws-paid

               move ws-rate1-int to ws-sales-rate1
               move ws-rate2-int to ws-sales-rate2

               write output-line from ws-detail-line
               add 1 to ws-line-count
           end-if.

      * Calculate percentage of employees with bonus...
       390-calc-percentages.
           if ws-count-employees > 0
               compute ws-percent-bonus =
                    (ws-count-bonus * 10000 / ws-count-employees) / 100
           else
               move 0 to ws-percent-bonus
           end-if.

      * Print summary...
       400-summary.
           write output-line from ws-spaceline
           write output-line from ws-spaceline

           move "Summary:" to output-line
           write output-line

           move "Overall Sales" to output-line
           move ws-total-sales to ws-sales-edited
           move ws-sales-edited to output-line
           write output-line

           move "Paid Total" to output-line
           move ws-total-paid to ws-paid-edited
           move ws-paid-edited to output-line
           write output-line

           move "Count of Employees" to output-line
           move ws-count-employees to ws-count-edited
           move ws-count-edited to output-line
           write output-line

           move "Bonus Count" to output-line
           move ws-count-bonus to ws-count-edited
           move ws-count-edited to output-line
           write output-line

           move "Percentage Bonus" to output-line
           move ws-percent-bonus-edited to output-line
           move "%" to output-line
           write output-line.

      * Close the files...
       500-close-files.
           close input-file
           close output-file.

       end program A3SCOMM.
