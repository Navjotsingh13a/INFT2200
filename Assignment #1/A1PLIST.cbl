       identification division.
       program-id. A1PLIST.
       date-written. 18 May 2025.
       author. Navjot Singh
      * Description: A1PLIST--> Program to print product list...

      *
       environment division.
       configuration section.
      *
       input-output section.
       file-control.

           select output-file
                assign to OUTFILE
                organization is sequential.

       data division.
       file section.

       fd output-file
           recording mode is F
           data record is output-line
           record contains 60 characters.

       01 output-line                       pic x(60).

       working-storage section.

      * Title...
       01 ws-title.
           05 filler               pic x(10) value spaces.
           05 filler               pic x(30)
              value "Mainframe I Product Listing".
           05 filler               pic x(20) value spaces.

      * Headings...
       01 ws-heading.
          05 filler               pic x(30)
             value "Product Name       Description".
          05 filler               pic x(30)
             value "                Quantity".

      * Structure: Product's Name, Description, Quantity....
       01 ws-detail-line.
           05 ws-product-name               pic x(15)
                value spaces.
           05 filler                        pic x(3)
                value spaces.
           05 ws-product-descr              pic x(30)
                value spaces.
           05 filler                        pic x(3)
                value spaces.
           05 ws-product-qnty               pic x(9)
                value spaces.

       01 ws-blank-line                     pic x(60)
                value spaces.



       procedure division.
       000-main.
      *
           open output output-file.

           write output-line                from ws-title.

           write output-line                from ws-blank-line.

           write output-line                from ws-heading.

           write output-line                from ws-blank-line.

      * First Product...
           move "PEN"                       to ws-product-name.
           move "Blue Ball Pen"             to ws-product-descr.
           move "000050"                    to ws-product-qnty.

           write output-line                from ws-detail-line.
           move spaces                      to ws-detail-line.


      * Second Product...
           move "BOOK"                      to ws-product-name.
           move "Notebook of 200 Pages"     to ws-product-descr.
           move "000030"                    to ws-product-qnty.

           write output-line                from ws-detail-line.
           move spaces                      to ws-detail-line.


      * Third Product...
           move "Box"                       to ws-product-name.
           move "Lunch Box Made of Plastic" to ws-product-descr.
           move "000050"                    to ws-product-qnty.

           write output-line                from ws-detail-line.
           move spaces                      to ws-detail-line.


      * Fourth Product...
           move "BAG"                       to ws-product-name.
           move "Small School Bag"          to ws-product-descr.
           move "000020"                    to ws-product-qnty.

           write output-line                from ws-detail-line.
           move spaces                      to ws-detail-line.


      * Fifth Product...
           move "BOTTLE"                    to ws-product-name.
           move "Water Bottle of 1L"        to ws-product-descr.
           move "000080"                    to ws-product-qnty.

           write output-line                from ws-detail-line.
           move spaces                      to ws-detail-line.


           close output-file.

           goback.
      *
       end program A1PLIST.
