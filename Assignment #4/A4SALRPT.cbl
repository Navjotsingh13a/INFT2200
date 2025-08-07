       identification division.
       program-id. A4SALRPT.
       date-written. June 20, 2025.
       author. Navjot Singh.
      * Description: This COBOL program creates a salary report,
      * calculating pay, increases, and averages based on employee
      * service years and education....

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

      * Input record Layout...
       fd input-file
           recording mode is F
           data record is input-line
           record contains 30 characters.
       01 input-line.
           05 il-Employee-Number            pic 9(4).
           05 il-Employee-Name              pic x(16).
           05 il-Educ-Code                  pic x(1).
           05 il-Year-Service               pic 9(2).
           05 il-Present-Salary             pic 9(5)v99.

      * Output record layout...
       fd output-file
           recording mode is F
           data record is output-line
           record contains 120 characters.
       01 output-line                       pic x(120).
       working-storage section.

       01 ws-educ-code                    pic x.
           88 graduate                value 'G'.
           88 non-graduate            value 'N'.

      * Report heading with name, date, time...
       01 ws-report-heading.
           05 filler                         pic x(34)
               value spaces.
           05 filler                         pic x(26)
               value "NAVJOT SINGH, ASSIGNMENT 4".
           05 filler                         pic x(10)
               value spaces.
           05 ws-date                        pic 9(8).
           05 filler                         pic x(10)
               value spaces.
           05 ws-time                        pic 9(8).

            05 filler                         pic x(25)
                value spaces.

      * Report Title...
       01 ws-report-title.
           05 filler                        pic x(37)
                value spaces.
           05 filler                        pic x(25)
                value "EMPLOYEE SALARY REPORT".
           05 filler                        pic x(10)
                value spaces.
           05 filler                        pic x(5)
               value "Page ".
       05 ws-report-page-no                 pic z9.

      * Column Heading for output...
       01 ws-column-headings.
           05 filler                         pic x(05)
                value spaces.
           05 filler                         pic x(03)
                value "EMP".
           05 filler                         pic x(05)
                value spaces.
           05 filler                         pic x(04)
                value "EMP.".
           05 filler                         pic x(05)
                value spaces.
           05 filler                         pic x(06)
                value spaces.
           05 filler                         pic x(04)
                value spaces.
           05 filler                         pic x(06)
                value spaces.
           05 filler                         pic x(01)
                value spaces.
           05 filler                         pic x(07)
                value "PRESENT".
           05 filler                         pic x(06)
                value spaces.
           05 filler                         pic x(08)
                value "INCREASE".
           05 filler                         pic x(07)
                value spaces.
           05 filler                         pic x(03)
                value "PAY".
           05 filler                         pic x(10)
                value spaces.
           05 filler                         pic x(03)
                value "NEW".

      * Column Heading 2 ...
       01 ws-column-heading2.
           05 filler                         pic x(05)
                value spaces.
           05 filler                         pic x(03)
                value "NO.".
           05 filler                         pic x(05)
                value spaces.
           05 filler                         pic x(04)
                value "NAME".
           05 filler                         pic x(12)
                value spaces.
           05 filler                         pic x(05)
                value "YEARS".
           05 filler                         pic x(05)
                value spaces.
           05 filler                         pic x(08)
                value "POSITION".
           05 filler                         pic x(06)
                value spaces.
           05 filler                         pic x(06)
                value "SALARY".
           05 filler                         pic x(09)
                value spaces.
           05 filler                         pic x(01)
                value "%".
           05 filler                         pic x(10)
                value spaces.
           05 filler                         pic x(12)
                value "INCREASE".
           05 filler                         pic x(02)
                value spaces.
           05 filler                         pic x(06)
                value "SALARY".

      * Detail - Line...
       01 ws-detail-line .
            05 filler                       pic x(05)
                value spaces.
            05 ws-Emp-Number                PIC 9(3).
            05 filler                       pic X(3)
                value spaces.
            05 ws-Emp-Name                  PIC X(15).
            05 filler                       pic X(03)
                value spaces.
            05 ws-Years                       PIC 9(2).
            05 filler                       pic X(7)
                value spaces.
            05 ws-Position                  PIC X(8).
            05 filler                       pic X(2)
                value spaces.
            05 ws-Present-Salary            PIC ZZZ,ZZ9.99.
            05 filler                       pic X(08)
                value spaces.
            05 ws-Inc-Prct-x.
                10 ws-Inc-Prct-Disp         pic zz.z
                     value spaces.
            05 ws-Percent-Sign              pic X(1)
                value "%".
            05 filler                       pic X(03)
                value spaces.
            05 ws-Pay-Increase              PIC $,$$$,$$9.99.
            05 filler                       PIC X(1)
                value "+".
            05 filler                       pic X(5)
                value spaces.
            05 ws-New-Salary                PIC $ZZZ,ZZ9.99.
      * Average...
         01 ws-Average1.
           05 filler                        pic x(05)
               value spaces.
            05 ws-Emp-Class                 PIC X(15)
               value "EMPLOYEE CLASS:".
            05 FILLER                       PIC X(10)
               value SPACES.
            05 ws-Avg-Analyst               PIC X(7)
               value "Analyst".
            05 FILLER                       PIC X(4)
               value SPACES.
            05 ws-Senprog-Avg               PIC X(8)
               value "Sen Prog".
            05 FILLER                       PIC X(4)
               value SPACES.
            05 ws-Prog-Avg                  PIC X(4)
               value "Prog".
            05 FILLER                       PIC X(4)
               value SPACES.
            05 ws-JrProg-Avg                PIC X(7)
               value "Jr Prog".
            05 FILLER                       PIC X(4)
               value SPACES.
            05 ws-Unclass-Avg               PIC X(12)
               value "Unclassified".
       01 ws-Average2.
           05 filler                        pic x(05)
               value spaces.
            05 ws-Num-Page-Count              pic x(15)
               value "ON THIS PAGE:".
            05 filler                       pic x(13)
               value spaces.
            05 ws-Count-Analyst             pic 9.
            05 filler                       pic x(10)
               value spaces.
            05 ws-Count-Senprog             pic 9.
            05 filler                       pic x(10)
               value spaces.
            05 ws-Count-Prog                pic 9.
            05 filler                       pic x(8)
               value spaces.
            05 ws-Count-JrProg              pic 9.
            05 filler                       pic x(8)
               value spaces.
            05 ws-Count-Unclass             pic 9.
       01 ws-calc-fields.
            05 ws-Present-Salary-Calc       pic 9(7)V99.
            05 ws-Pay-Increase-Calc         pic 9(7)V99.
            05 ws-New-Salary-Calc           pic 9(7)V99.
            05 ws-Increase-Percent-Calc     pic 9(2)V9.
       01 ws-Average-Variable1.
           05 filler                        pic x(05)
                value spaces.
            05 ws-Avg-Increase              pic X(16)
                 value "AVRAGE INCREAES:".
            05 filler                       pic X(07)
                 value spaces.
            05 ws-Analyst-Inc-Avg           pic X(08)
                 value "ANALYST=".
            05 filler                       pic x(07)
                 value spaces.
            05 ws-Analyst-Avg-Display       PIC ZZ,ZZ9.99.
            05 filler                       pic x(07)
                 value spaces.
            05 ws-Senprog-Inc-Avg           pic X(09)
                 value "SEN PROG=".
            05 filler                       pic x(07)
                 value spaces.
            05 ws-Senprog-Avg-Display       PIC ZZ,ZZ9.99.
       01 ws-Average-Variable2.
            05 filler                       pic x(24)
                 value spaces.
            05 ws-Prog-Avg-Inc              pic x(05)
                 value "PROG=".
            05 filler                       pic x(10)
                 value spaces.
            05 ws-Prog-Avg-Display          pic ZZ,ZZ9.99.
            05 filler                       pic x(07)
                 value spaces.
            05 ws-JrProg-Avg-Inc            pic x(08)
                 value "JR PROG=".
            05 filler                       pic x(07)
                 value spaces.
            05 ws-JrProg-Avg-Display        pic ZZ,ZZ9.99.
       01 ws-Totals.
           05 ws-Analyst-Total              pic 9(9)V99
               value 0.
           05 ws-Senprog-Total              pic 9(9)V99
               value 0.
           05 ws-Prog-Total                 pic 9(9)V99
               value 0.
           05 ws-JrProg-Total               pic 9(9)V99
               value 0.
           05 ws-Analyst-Total-Count        pic 9(3)
               value 0.
           05 ws-SenProg-Total-Count        pic 9(3)
               value 0.
           05 ws-Prog-Total-Count           pic 9(3)
               value 0.
           05 ws-JrProg-Total-Count         pic 9(3)
               value 0.
       01 ws-Average-Values.
           05 ws-Analyst-Avg-Total          pic 9(9)V99
               value 0.
           05 ws-Senprog-Avg-Total          pic 9(9)V99
               value 0.
           05 ws-Prog-Avg-Total             pic 9(9)V99
               value 0.
           05 ws-JrProg-Avg-Total           pic 9(9)V99
               value 0.
       01 ws-Total-Calculation.

           05 ws-Total-Analyst-Format       pic zzz,zzz.99.
           05 ws-Total-SenProg-format       pic zzz,zzz.99.
           05 ws-Total-Prg-Format           pic zzz,zzz.99.
           05 ws-Total-JrPrg-Format         pic zzz,zzz.99.
       77 ws-Inc-Prct-Alpha-Display         pic x(4) value spaces.
       77 ws-eof-flag                       pic x value 'n'.
       77 ws-const-y                        pic x value 'y'.
       77 ws-Page-Number                    pic 9 value 0.
       77 ws-Max-Pages                      pic 9 value 5.
       77 ws-line-count                     pic 99 value 0.
       77 ws-lines-per-page                 pic 99 value 10.
       77 ws-spaceline                      pic x(99) value spaces.
       77 ws-const-zero                     pic 9 value 0.
       77 ws-const-one                      pic 9 value 1.
       77 ws-Analyst-const                  pic x(7) value "ANALYST".
       77 ws-SenProg-const                  pic x(8) value "SEN PROG".
       77 ws-Prog-const                     pic x(4) value "PROG".
       77 ws-Unclass-const                  pic x(8) value "UN-CLAS.".
       77 ws-Un-Prog                        pic x(4) value "PROG".
       77 ws-Jr-Prog-const                  pic x(7) value "JR PROG".
       77 ws-Analyst-Inc                    pic 99V9 value 13.8.
       77 ws-SenProg-Inc                    pic 99V9
            value 10.3.
       77 ws-Prog-Inc                       pic 9V9
            value 7.7.
       77 ws-Jr-Prog-Inc                    pic 9V9
            value 4.2.
       77 ws-Unclass-Inc                    pic 9(1)
            value 0.
       77 ws-Last-Page-flag                 pic x value 'N'.

       procedure division.
       000-main.

      * Open files...
           open input input-file.
           open output output-file.

      * Date and Time...
           accept ws-date from date yyyymmdd.
               display ws-date.
           accept ws-time from time.
                display ws-time.
           move 0 to ws-Page-Number.
           perform 100-read-files.
           perform 150-paging-process
               until ws-eof-flag = ws-const-y
               or ws-Page-Number >= ws-Max-Pages.
           perform 300-print-last-page.

      * Close the input and output files...
           close input-file output-file.
           goback.
       100-read-files.

           read input-file
               at end move ws-const-y       to ws-eof-flag.

      * Start new page, count jobs, calculate, and print averages...
       150-paging-process.
           move 0 to ws-line-count.
           perform 200-headings.
           move 0                           to ws-Count-Analyst
           move 0                           to ws-Count-SenProg
           move 0                           to ws-Count-Prog
           move 0                           to ws-Count-JrProg
           move 0                           to ws-Count-Unclass
           perform 250-calculation-sale
               until ws-eof-flag = ws-const-y
               or ws-line-count >= ws-lines-per-page.
           write output-line                from ws-spaceline.
           write output-line                from ws-Average1.
           write output-line                from ws-Average2.

      * Print page number, headings, and column titles...
       200-headings.
           add 1 to ws-Page-Number.
           if ws-Page-Number > ws-Max-Pages
               move 1 to ws-Page-Number
           end-if.
           move ws-Page-Number              to ws-report-page-no.
           if ws-Page-Number = 1
               write output-line            from ws-report-heading
               write output-line            from ws-spaceline
               write output-line            from ws-report-title
           else
               write output-line            from ws-spaceline
               write output-line            from ws-report-title
                   after advancing page
           end-if.
           write output-line                from ws-spaceline.
           write output-line                from ws-spaceline.
           write output-line                from ws-column-headings.
           write output-line                from ws-column-heading2.

      * Set position by education and service,
      * calculate increase and total...
       250-calculation-sale.
           move  il-Employee-Number    to  ws-Emp-Number.
           move  il-Employee-Name      to  ws-Emp-Name.
           move  il-Year-Service      to  ws-Years.
           move  il-Present-Salary     to  ws-Present-Salary-Calc.
           move  "%"                   to ws-Percent-Sign.
           if il-Educ-Code = "G" and il-Year-Service > 16 then
                  move ws-Analyst-const TO ws-Position
           else
           if il-Educ-Code = "G" and il-Year-Service >= 5 and
                  il-Year-Service <= 16 then
                  move ws-SenProg-const to ws-Position
             else
             if il-Educ-Code = "G" and il-Year-Service <= 4 then

                  move ws-Unclass-const to ws-Position
             else
             if il-Educ-Code = "N" and  il-Year-Service > 9  then
                  move ws-Prog-const to ws-Position
             else
             if il-Educ-Code = "N" and il-Year-Service > 4 and
                il-Year-Service <= 9 then
                  move ws-Jr-Prog-const to ws-Position
             else
                 if il-Educ-Code = "N" and il-Year-Service <= 4 then
                      move ws-Unclass-const to ws-Position
                 end-if
            end-if.
      * Add employee data to correct totals based on job position...
           if ws-Position = ws-Analyst-const then
                 add 1 to  ws-Count-Analyst
                 add 1 to ws-Analyst-Total-Count of ws-Totals
                 add ws-Pay-Increase-Calc
                                    to ws-Analyst-Total of ws-Totals
           else if ws-Position = ws-SenProg-const then
                 add 1 to ws-count-senprog
                 add 1 to ws-SenProg-Total-Count of ws-Totals
                 add ws-Pay-Increase-Calc
                                    to ws-Senprog-Total of ws-Totals
           else if ws-Position = ws-Prog-const then
                 add 1 to ws-Count-Prog
                 add 1 to ws-Prog-Total-Count of ws-Totals
                 add ws-Pay-Increase-Calc
                                    to ws-Prog-Total of ws-Totals
           else if ws-Position = ws-Jr-Prog-const then
                 add 1 to ws-Count-JrProg
                 add 1 to ws-JrProg-Total-Count of ws-Totals
                 add ws-Pay-Increase-Calc
                                    to ws-JrProg-Total of ws-Totals
           else if ws-Position = ws-Unclass-const then
                 add 1 to ws-Count-Unclass
           end-if.
            if ws-Position = ws-Analyst-const then
                 move ws-Analyst-inc to ws-Increase-Percent-Calc
            else if ws-Position = ws-SenProg-const then
                 move ws-SenProg-Inc to ws-Increase-Percent-Calc
            else if ws-Position = ws-Prog-const then
                 move ws-Prog-Inc to ws-Increase-Percent-Calc
            else if ws-Position = ws-Jr-Prog-const then
                 move ws-Jr-Prog-Inc to ws-Increase-Percent-Calc
            else
                 move 0 to ws-Increase-Percent-Calc
                 move spaces to ws-Percent-Sign
            end-if.
            if ws-Increase-Percent-Calc = 0
                move spaces to ws-Inc-Prct-x
            else
                move ws-Increase-Percent-Calc to ws-Inc-Prct-Disp
            end-if.
           compute ws-Pay-Increase-Calc rounded
                = (il-Present-Salary * ws-Increase-Percent-Calc)
                                                     / 100
           compute ws-New-Salary-Calc rounded
                = (il-Present-Salary + ws-Pay-Increase-Calc)
           move ws-Present-Salary-Calc to ws-Present-Salary
           move ws-Increase-Percent-Calc to ws-Inc-Prct-Disp
           move ws-Pay-Increase-Calc to ws-Pay-Increase
           move ws-New-Salary-Calc to ws-New-Salary
           write output-line                from ws-spaceline .
           write output-line                from ws-detail-line .
           add 1 to ws-line-count.
           read input-file
               at end
                   move ws-const-y to ws-eof-flag
                   move 'Y' to ws-Last-Page-flag.

      * Prints the final page with averages if it is the last page...
       300-print-last-page.
           if ws-Last-Page-flag = 'Y'
               perform 350-calculate-averages
               write output-line            from ws-spaceline
               write output-line            from ws-Average-Variable1
               write output-line            from ws-Average-Variable2
           end-if.

      * Calculate average salary for each job position...
       350-calculate-averages.
           if ws-Analyst-Total-Count > 0
             compute ws-Analyst-Avg-Total rounded
                 = (ws-Analyst-Total / ws-Analyst-Total-Count)
             move ws-Analyst-Avg-Total to ws-Total-Analyst-Format
             move ws-Total-Analyst-Format to ws-Analyst-Avg-Display
             of ws-Average-Variable1
           else
            move zero to ws-Analyst-Avg-Display of ws-Average-Variable1
           end-if.
           if ws-SenProg-Total-Count > 0
            compute ws-Senprog-Avg-Total rounded
                = (ws-Senprog-Total / ws-SenProg-Total-Count)
            move ws-Senprog-Avg-Total to ws-Total-SenProg-format
            move ws-Total-SenProg-format to ws-Senprog-Avg-Display of
             ws-Average-Variable1
           else
            move zero to ws-Senprog-Avg-Display of ws-Average-Variable1
           end-if.
           if ws-Prog-Total-Count > 0
            compute ws-Prog-Avg-Total rounded
                = (ws-Prog-Total / ws-Prog-Total-Count)
            move ws-Prog-Avg-Total to ws-Total-Prg-Format
            move ws-Total-Prg-Format to ws-Prog-Avg-Display of
             ws-Average-Variable2
            else
            move zero to ws-Prog-Avg-Display of ws-Average-Variable2
           end-if.
           if ws-JrProg-Total-Count > 0
            compute ws-JrProg-Avg-Total rounded
                = (ws-JrProg-Total / ws-JrProg-Total-Count)
            move ws-JrProg-Avg-Total to ws-Total-JrPrg-Format
            move ws-Total-JrPrg-Format to ws-JrProg-Avg-Display of
             ws-Average-Variable2
           else
            move zero to ws-JrProg-Avg-Display of ws-Average-Variable2
           end-if.

       end program A4SALRPT.