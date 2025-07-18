CLASS zcl_date DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZCL_DATE
*"* do not include other source files here!!!
  PUBLIC SECTION.

    "! Gets short month name by month number
    "! @parameter iv_number | Month number (1-12)
    "! @parameter rv_short_month | Short month name
    METHODS get_short_month_name_by_number
      IMPORTING
        !iv_number            TYPE fcmnr
      RETURNING
        VALUE(rv_short_month) TYPE fcktx .
    "! Gets full month name by month number
    "! @parameter iv_number | Month number (1-12)
    "! @parameter rv_month_name | Full month name
    METHODS get_month_name_by_number
      IMPORTING
        !iv_number           TYPE fcmnr
      RETURNING
        VALUE(rv_month_name) TYPE fcltx .
    "! Constructor for date object
    "! @parameter iv_date | Date value (default: current date)
    "! @parameter iv_language | Language key (default: current language)
    METHODS constructor
      IMPORTING
        !iv_date     LIKE sy-datum DEFAULT sy-datum
        !iv_language LIKE sy-langu DEFAULT sy-langu
          PREFERRED PARAMETER iv_date .
    "! Sets the date value
    "! @parameter iv_date | New date value
    METHODS set_date
      IMPORTING
        !iv_date TYPE dats .
    "! Gets the current date value
    "! @parameter rv_date | Current date value
    METHODS get_date
      RETURNING
        VALUE(rv_date) TYPE dats .
    "! Sets the day component of the date
    "! @parameter iv_day | Day value (default: 01)
    METHODS set_day
      IMPORTING
        !iv_day TYPE num2 DEFAULT 01 .
    "! Gets the day component of the date
    "! @parameter rv_day | Day component
    METHODS get_day
      RETURNING
        VALUE(rv_day) TYPE num2 .
    "! Gets the full month name for current date
    "! @parameter rv_month_name | Full month name
    METHODS get_month_name
      RETURNING
        VALUE(rv_month_name) TYPE fcltx .
    "! Gets the short month name for current date
    "! @parameter rv_short_month | Short month name
    METHODS get_short_month_name
      RETURNING
        VALUE(rv_short_month) TYPE fcktx .
    "! Sets the month number component of the date
    "! @parameter iv_month_number | Month number (default: 01)
    METHODS set_month_number
      IMPORTING
        !iv_month_number TYPE num2 DEFAULT 01 .
    "! Gets the month number component of the date
    "! @parameter rv_month_number | Month number
    METHODS get_month_number
      RETURNING
        VALUE(rv_month_number) TYPE num2 .
    "! Gets the short year (2 digits) component of the date
    "! @parameter rv_short_year | Short year (2 digits)
    METHODS get_short_year
      RETURNING
        VALUE(rv_short_year) TYPE num2 .
    "! Sets the year component of the date
    "! @parameter iv_year | Year value (default: 0001)
    METHODS set_year
      IMPORTING
        !iv_year TYPE num4 DEFAULT 0001 .
    "! Gets the year component of the date
    "! @parameter rv_year | Year component
    METHODS get_year
      RETURNING
        VALUE(rv_year) TYPE num4 .
    "! Converts date to formatted string
    "! @parameter iv_format | Format string (default: '$dd $m $yyyy')
    "! @parameter rv_string | Formatted date string
    METHODS to_string
      IMPORTING
        VALUE(iv_format) TYPE string DEFAULT '$dd $m $yyyy'
      RETURNING
        VALUE(rv_string) TYPE string .
    "! Factory method to create date instance
    "! @parameter iv_date | Date value (default: current date)
    "! @parameter iv_language | Language key (default: current language)
    "! @parameter ro_instance | Created date instance
    CLASS-METHODS create
      IMPORTING
        !iv_date           LIKE sy-datum DEFAULT sy-datum
        !iv_language       LIKE sy-langu DEFAULT sy-langu
          PREFERRED PARAMETER iv_date
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_date .
    "! Calculates difference between two date objects
    "! @parameter iv_dat1 | First date object
    "! @parameter iv_dat2 | Second date object
    "! @parameter rv_diff | Difference in days
    CLASS-METHODS obj_diff
      IMPORTING
        !iv_dat1       TYPE REF TO zcl_date
        !iv_dat2       TYPE REF TO zcl_date
      RETURNING
        VALUE(rv_diff) TYPE i .
    "! Calculates difference between two dates
    "! @parameter iv_dats1 | First date
    "! @parameter iv_dats2 | Second date
    "! @parameter rv_diff | Difference in days
    CLASS-METHODS diff
      IMPORTING
        !iv_dats1      TYPE dats
        !iv_dats2      TYPE dats
      RETURNING
        VALUE(rv_diff) TYPE i .
    "! Subtracts days, months, or years from current date
    "! @parameter iv_days | Number of days to subtract (default: 00)
    "! @parameter iv_months | Number of months to subtract (default: 00)
    "! @parameter iv_years | Number of years to subtract (default: 00)
    METHODS minus
      IMPORTING
        !iv_days   TYPE num2 DEFAULT 00
        !iv_months TYPE num2 DEFAULT 00
        !iv_years  TYPE num2 DEFAULT 00 .
    "! Adds days, months, or years to current date
    "! @parameter iv_days | Number of days to add (default: 00)
    "! @parameter iv_months | Number of months to add (default: 00)
    "! @parameter iv_years | Number of years to add (default: 00)
    METHODS plus
      IMPORTING
        !iv_days   TYPE num2 DEFAULT 00
        !iv_months TYPE num2 DEFAULT 00
        !iv_years  TYPE num2 DEFAULT 00 .
    "! Gets the current language setting
    "! @parameter rv_language | Current language
    METHODS get_language
      RETURNING
        VALUE(rv_language) TYPE lang .
    "! Sets the language for date formatting
    "! @parameter iv_language | Language key
    METHODS set_language
      IMPORTING
        !iv_language TYPE lang .
*"* protected components of class ZCL_DATE
*"* do not include other source files here!!!
  PROTECTED SECTION.

*"* private components of class ZCL_DATE
*"* do not include other source files here!!!
  PRIVATE SECTION.

    DATA:
      gt_month_names TYPE STANDARD TABLE OF t247 .
    DATA gv_language TYPE lang .
    DATA gv_date TYPE dats .
    CONSTANTS nv_long_year TYPE string VALUE '$yyyy'.       "#EC NOTEXT
    CONSTANTS nv_short_year TYPE string VALUE '$yy'.        "#EC NOTEXT
    CONSTANTS nv_month_name TYPE string VALUE '$m'.         "#EC NOTEXT
    CONSTANTS nv_short_month_name TYPE string VALUE '$mmm'. "#EC NOTEXT
    CONSTANTS nv_month_number TYPE string VALUE '$mm'.      "#EC NOTEXT
    CONSTANTS nv_day TYPE string VALUE '$dd'.               "#EC NOTEXT

    "! Initializes month names table for current language
    METHODS init_month_names .
    "! Performs date calculation (add/subtract days/months/years)
    "! @parameter iv_days | Number of days (default: 00)
    "! @parameter iv_months | Number of months (default: 00)
    "! @parameter iv_years | Number of years (default: 00)
    "! @parameter iv_sign | Sign for calculation (default: '+')
    METHODS date_calculation
      IMPORTING
        !iv_days   TYPE t5a4a-dlydy DEFAULT 00
        !iv_months TYPE t5a4a-dlymo DEFAULT 00
        !iv_years  TYPE t5a4a-dlyyr DEFAULT 00
        !iv_sign   TYPE t5a4a-split DEFAULT '+' .
ENDCLASS.



CLASS ZCL_DATE IMPLEMENTATION.

  "! Constructor implementation
  "! @parameter iv_language | Language for date formatting
  "! @parameter iv_date | Initial date value
  METHOD constructor.
    gv_language = iv_language.
    gv_date = iv_date.
  ENDMETHOD.

  "! Factory method implementation
  "! @parameter iv_date | Date value for new instance
  "! @parameter iv_language | Language for new instance
  "! @parameter ro_instance | Created date instance
  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_date     = iv_date
        iv_language = iv_language.
  ENDMETHOD.

  "! Date calculation implementation
  "! @parameter iv_days | Days to add/subtract
  "! @parameter iv_months | Months to add/subtract
  "! @parameter iv_years | Years to add/subtract
  "! @parameter iv_sign | Sign for calculation
  METHOD date_calculation.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = gv_date
        days      = iv_days
        months    = iv_months
        signum    = iv_sign
        years     = iv_years
      IMPORTING
        calc_date = gv_date.
  ENDMETHOD.

  "! Date difference calculation implementation
  "! @parameter iv_dats1 | First date
  "! @parameter iv_dats2 | Second date
  "! @parameter rv_diff | Difference in days
  METHOD diff.
    rv_diff = iv_dats1 - iv_dats2.
  ENDMETHOD.

  "! Get date implementation
  "! @parameter rv_date | Current date value
  METHOD get_date.
    rv_date = gv_date.
  ENDMETHOD.

  "! Get day implementation
  "! @parameter rv_day | Day component of date
  METHOD get_day.
    rv_day = gv_date+6(2).
  ENDMETHOD.

  "! Get language implementation
  "! @parameter rv_language | Current language setting
  METHOD get_language.
    rv_language = gv_language.
  ENDMETHOD.

  "! Get month name implementation
  "! @parameter rv_month_name | Full month name for current date
  METHOD get_month_name.
    DATA lv_month_num TYPE fcmnr.
    lv_month_num = get_month_number( ).
    rv_month_name = get_month_name_by_number( lv_month_num ).
  ENDMETHOD.

  "! Get month name by number implementation
  "! @parameter iv_number | Month number
  "! @parameter rv_month_name | Full month name
  METHOD get_month_name_by_number.
    DATA ls_month LIKE LINE OF gt_month_names.
  
    " Есть ли вообще записи для текущего языка?
    READ TABLE gt_month_names
      INTO ls_month
      WITH KEY spras = gv_language.
  
    IF sy-subrc <> 0.
      init_month_names( ).
    ENDIF.
  
    CLEAR ls_month.
    READ TABLE gt_month_names
      INTO ls_month
      WITH KEY
        mnr   = iv_number
        spras = gv_language.
  
    rv_month_name = ls_month-ltx.
  ENDMETHOD.

  "! Get month number implementation
  "! @parameter rv_month_number | Month number component
  METHOD get_month_number.
    rv_month_number = gv_date+4(2).
  ENDMETHOD.

  "! Get short month name implementation
  "! @parameter rv_short_month | Short month name for current date
  METHOD get_short_month_name.
    DATA lv_month_num TYPE fcmnr.
    lv_month_num = get_month_number( ).
    rv_short_month = get_short_month_name_by_number( lv_month_num ).
  ENDMETHOD.

  "! Get short month name by number implementation
  "! @parameter iv_number | Month number
  "! @parameter rv_short_month | Short month name
  METHOD get_short_month_name_by_number.
    DATA ls_month LIKE LINE OF gt_month_names.
  
    " Есть ли вообще записи для текущего языка?
    READ TABLE gt_month_names
      INTO ls_month
      WITH KEY spras = gv_language.
  
    IF sy-subrc <> 0.
      init_month_names( ).
    ENDIF.
  
    CLEAR ls_month.
    READ TABLE gt_month_names
      INTO ls_month
      WITH KEY
        mnr   = iv_number
        spras = gv_language.
  
    rv_short_month = ls_month-ktx.
  ENDMETHOD.

  "! Get short year implementation
  "! @parameter rv_short_year | Short year (2 digits)
  METHOD get_short_year.
    rv_short_year = gv_date+2(2).
  ENDMETHOD.

  "! Get year implementation
  "! @parameter rv_year | Year component
  METHOD get_year.
    rv_year = gv_date+0(4).
  ENDMETHOD.

  "! Initialize month names implementation
  METHOD init_month_names.
    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language              = gv_language
      TABLES
        month_names           = gt_month_names
      EXCEPTIONS
        month_names_not_found = 1
        OTHERS                = 2.
  ENDMETHOD.

  "! Minus operation implementation
  "! @parameter iv_days | Days to subtract
  "! @parameter iv_months | Months to subtract
  "! @parameter iv_years | Years to subtract
  METHOD minus.
    date_calculation( iv_days   = iv_days
                      iv_months = iv_months
                      iv_years  = iv_years
                      iv_sign   = '-' ).
  ENDMETHOD.

  "! Object difference calculation implementation
  "! @parameter iv_dat1 | First date object
  "! @parameter iv_dat2 | Second date object
  "! @parameter rv_diff | Difference in days
  METHOD obj_diff.
    DATA:
      lv_dats1 TYPE dats,
      lv_dats2 TYPE dats.

    lv_dats1 = iv_dat1->get_date( ).
    lv_dats2 = iv_dat2->get_date( ).

    rv_diff = lv_dats1 - lv_dats2.
  ENDMETHOD.

  "! Plus operation implementation
  "! @parameter iv_days | Days to add
  "! @parameter iv_months | Months to add
  "! @parameter iv_years | Years to add
  METHOD plus.
    date_calculation( iv_days   = iv_days
                      iv_months = iv_months
                      iv_years  = iv_years
                      iv_sign   = '+' ).
  ENDMETHOD.

  "! Set date implementation
  "! @parameter iv_date | New date value
  METHOD set_date.
    gv_date = iv_date.
  ENDMETHOD.

  "! Set day implementation
  "! @parameter iv_day | New day value
  METHOD set_day.
    gv_date+6(2) = iv_day.
  ENDMETHOD.

  "! Set language implementation
  "! @parameter iv_language | New language setting
  METHOD set_language.
    gv_language = iv_language.
  ENDMETHOD.

  "! Set month number implementation
  "! @parameter iv_month_number | New month number
  METHOD set_month_number.
    gv_date+4(2) = iv_month_number.
  ENDMETHOD.

  "! Set year implementation
  "! @parameter iv_year | New year value
  METHOD set_year.
    gv_date+0(4) = iv_year.
  ENDMETHOD.

  "! To string implementation
  "! @parameter iv_format | Format string for date conversion
  "! @parameter rv_string | Formatted date string
  METHOD to_string.
    DATA:
      lv_day              TYPE num2,
      lv_month_name       TYPE string,
      lv_month_number     TYPE num2,
      lv_short_month_name TYPE string,
      lv_short_year       TYPE num2,
      lv_long_year        TYPE num4.

    lv_day = get_day( ).
    lv_month_name = get_month_name( ).
    lv_month_number = get_month_number( ).
    lv_short_month_name = get_short_month_name( ).
    lv_short_year = get_short_year( ).
    lv_long_year = get_year( ).

    " year
    REPLACE ALL OCCURRENCES OF nv_long_year IN iv_format WITH lv_long_year IGNORING CASE.
    REPLACE ALL OCCURRENCES OF nv_short_year IN iv_format WITH lv_short_year IGNORING CASE.

    " month
    REPLACE ALL OCCURRENCES OF nv_short_month_name IN iv_format WITH lv_short_month_name IGNORING CASE.
    REPLACE ALL OCCURRENCES OF nv_month_number IN iv_format WITH lv_month_number IGNORING CASE.
    REPLACE ALL OCCURRENCES OF nv_month_name IN iv_format WITH lv_month_name IGNORING CASE.

    "day
    REPLACE ALL OCCURRENCES OF nv_day IN iv_format WITH lv_day IGNORING CASE.

    rv_string = iv_format.
  ENDMETHOD.
ENDCLASS.
