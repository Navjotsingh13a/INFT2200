       identification division.
       program-id. A2STOCK.
       date-written. 28 May 2025.
       author. Navjot Singh
      * Description: This program reads stock data and generates a
      * profit report....

       environment division.
       input-output section.
       file-control.
           select input-file assign to INFILE.
           select output-file assign to OUTFILE.

       data division.
       file section.

       fd input-file
           recording mode is F.

      * Structure of an input record...
       01 input-record.
           05 in-stock-number     pic 9(7).
           05 in-item-number      pic 9(5).
           05 in-product-dept     pic x(7).
           05 in-location         pic x(4).
           05 in-qty-on-hand      pic 9(5).
           05 in-ordered-qty      pic 9(5).
           05 in-price-per-unit   pic 9(7).

      * Output file description....
       fd output-file
           recording mode is F.
       01 output-record          pic x(132).

       working-storage section.
       01 eof-flag         pic x value 'N'.

       01 ws-total-final-profit        pic 9(11)v99 value 0.
       01 ws-edit-total-final-profit   pic $$$,$$$,$$$,$$9.99.

      * Report Heading...
       01 ws-report-heading.
           05 filler pic x(132) value
              'Navjot Singh - Assignment 2'.

      * Column Titles...
       01 ws-title.
           05 filler pic x(8)  value "Stock  ".
           05 filler pic x(6)  value "Item ".
           05 filler pic x(9)  value " Dept   ".
           05 filler pic x(6)  value "Loc ".
           05 filler pic x(13) value "Qty On Hand".
           05 filler pic x(13) value "Ordered Qty".
           05 filler pic x(14) value "Need To Order".
           05 filler pic x(16) value "Price/Unit".
           05 filler pic x(21) value "Profit Before Disc".
           05 filler pic x(11) value "Discount".
           05 filler pic x(14) value "Final Profit".

      * Detail record layout for displaying stock and profit information
      * in report....

       01 detail-record.
           05 d-stock-number       pic x(7).
           05 filler               pic x(1) value space.
           05 d-item-number        pic x(5).
           05 filler               pic x(1) value space.
           05 d-product-dept       pic x(7).
           05 filler               pic x(2) value space.
           05 d-location           pic x(4).
           05 filler               pic x(2) value space.
           05 d-qty-on-hand        pic zz,zzz.
           05 filler               pic x(4) value space.
           05 d-ordered-qty        pic zz,zzz.
           05 filler               pic x(3) value space.
           05 d-need-to-order      pic zz,zzz.
           05 filler               pic x(3) value space.
           05 d-unit-price         pic zz,zz9.99.
           05 filler               pic x(3) value space.
           05 d-before-disc        pic zz,zz9.99.
           05 filler               pic x(3) value space.
           05 d-discount           pic z9.
           05 filler               pic x(8) value space.
           05 d-final-profit       pic zz,zz9.99.

       01 blank-line.
           05 filler pic x(132) value spaces.

      * Total Final Profit in the report...
       01 total-line.
           05 filler              pic x(19) value 'Total Final Profit:'.
           05 filler               pic x(3)  value spaces.
           05 total-amount         pic zzz,zz9.99.
           05 filler               pic x(103) value spaces.

      * Variables used for calculations in processing each record...
       01 calculations.
           05 ws-price             pic 9(7)v99.
           05 ws-need              pic s9(5).
           05 ws-before-disc       pic 9(7)v99.
           05 ws-discount          pic 99.
           05 ws-final             pic 9(7)v99.

       procedure division.
       000-main.
           open input input-file
           open output output-file

      * Write report heading and titles...
           move ws-report-heading to output-record
           write output-record

           move blank-line to output-record
           write output-record

           move ws-title to output-record
           write output-record

           move blank-line to output-record
           write output-record

      * Loop through input file until end of file...
           perform until eof-flag = 'Y'
               read input-file
                   at end
                       move 'Y' to eof-flag
                   not at end
                       perform process-record
           end-read
           end-perform.
           move blank-line to output-record
           write output-record

      * Write blank line and total final profit line...
           move ws-total-final-profit to total-amount
           move total-line to output-record
           write output-record

      * Stop the program...
           close input-file
           close output-file
           goback.

       process-record.

      * Calculate price per unit and needed quantity...
           compute ws-price = in-price-per-unit / 100.00
           compute ws-need = in-ordered-qty - in-qty-on-hand

      * Calculate profit before discount...
           compute ws-before-disc = in-ordered-qty * ws-price

      * Set discount based on product department....
           if in-product-dept = "B1     " or in-product-dept = "D1     "
               move 11 to ws-discount
           else
               move 0 to ws-discount
           end-if

      * Calculate final profit after discount....
           compute ws-final = ws-before-disc * (100 - ws-discount) / 100

      * Add final profit to total...
           add ws-final to ws-total-final-profit

      * Move values to detail record fields....
           move in-stock-number to d-stock-number
           move in-item-number to d-item-number
           move in-product-dept to d-product-dept
           move in-location to d-location
           move in-qty-on-hand to d-qty-on-hand
           move in-ordered-qty to d-ordered-qty
           move ws-need to d-need-to-order
           move ws-price to d-unit-price
           move ws-before-disc to d-before-disc
           move ws-discount to d-discount
           move ws-final to d-final-profit

      * Write output...
           move detail-record to output-record
           write output-record.

           move blank-line to output-record
           write output-record.


      * Terminate program...
       end program A2STOCK.