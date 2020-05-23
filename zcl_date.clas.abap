CLASS zcl_date DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZCL_DATE
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_short_month_name_by_number
      IMPORTING
        !iv_number            TYPE fcmnr
      RETURNING
        VALUE(rv_short_month) TYPE fcktx .
    METHODS get_month_name_by_number
      IMPORTING
        !iv_number           TYPE fcmnr
      RETURNING
        VALUE(rv_month_name) TYPE fcltx .
    METHODS constructor
      IMPORTING
        !iv_date     LIKE sy-datum DEFAULT sy-datum
        !iv_language LIKE sy-langu DEFAULT sy-langu
          PREFERRED PARAMETER iv_date .
    METHODS set_date
      IMPORTING
        !iv_date TYPE dats .
    METHODS get_date
      RETURNING
        VALUE(rv_date) TYPE dats .
    METHODS set_day
      IMPORTING
        !iv_day TYPE num2 DEFAULT 01 .
    METHODS get_day
      RETURNING
        VALUE(rv_day) TYPE num2 .
    METHODS get_month_name
      RETURNING
        VALUE(rv_month_name) TYPE fcltx .
    METHODS get_short_month_name
      RETURNING
        VALUE(rv_short_month) TYPE fcktx .
    METHODS set_month_number
      IMPORTING
        !iv_month_number TYPE num2 DEFAULT 01 .
    METHODS get_month_number
      RETURNING
        VALUE(rv_month_number) TYPE num2 .
    METHODS get_short_year
      RETURNING
        VALUE(rv_short_year) TYPE num2 .
    METHODS set_year
      IMPORTING
        !iv_year TYPE num4 DEFAULT 0001 .
    METHODS get_year
      RETURNING
        VALUE(rv_year) TYPE num4 .
    METHODS to_string
      IMPORTING
        VALUE(iv_format) TYPE string DEFAULT '$dd $m $yyyy'
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS create
      IMPORTING
        !iv_date           LIKE sy-datum DEFAULT sy-datum
        !iv_language       LIKE sy-langu DEFAULT sy-langu
          PREFERRED PARAMETER iv_date
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_date .
    CLASS-METHODS obj_diff
      IMPORTING
        !iv_dat1       TYPE REF TO zcl_date
        !iv_dat2       TYPE REF TO zcl_date
      RETURNING
        VALUE(rv_diff) TYPE i .
    CLASS-METHODS diff
      IMPORTING
        !iv_dats1      TYPE dats
        !iv_dats2      TYPE dats
      RETURNING
        VALUE(rv_diff) TYPE i .
    METHODS minus
      IMPORTING
        !iv_days   TYPE num2 DEFAULT 00
        !iv_months TYPE num2 DEFAULT 00
        !iv_years  TYPE num2 DEFAULT 00 .
    METHODS plus
      IMPORTING
        !iv_days   TYPE num2 DEFAULT 00
        !iv_months TYPE num2 DEFAULT 00
        !iv_years  TYPE num2 DEFAULT 00 .
    METHODS get_language
      RETURNING
        VALUE(rv_language) TYPE lang .
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

    METHODS init_month_names .
    METHODS date_calculation
      IMPORTING
        !iv_days   TYPE t5a4a-dlydy DEFAULT 00
        !iv_months TYPE t5a4a-dlymo DEFAULT 00
        !iv_years  TYPE t5a4a-dlyyr DEFAULT 00
        !iv_sign   TYPE t5a4a-split DEFAULT '+' .
ENDCLASS.



CLASS ZCL_DATE IMPLEMENTATION.


  METHOD constructor.
    gv_language = iv_language.
    gv_date = iv_date.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_date     = iv_date
        iv_language = iv_language.
  ENDMETHOD.


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


  METHOD diff.
    rv_diff = iv_dats1 - iv_dats2.
  ENDMETHOD.


  METHOD get_date.
    rv_date = gv_date.
  ENDMETHOD.


  METHOD get_day.
    rv_day = gv_date+6(2).
  ENDMETHOD.


  METHOD get_language.
    rv_language = gv_language.
  ENDMETHOD.


  METHOD get_month_name.
    DATA lv_month_num TYPE fcmnr.
    lv_month_num = get_month_number( ).
    rv_month_name = get_month_name_by_number( lv_month_num ).
  ENDMETHOD.


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


  METHOD get_month_number.
    rv_month_number = gv_date+4(2).
  ENDMETHOD.


  METHOD get_short_month_name.
    DATA lv_month_num TYPE fcmnr.
    lv_month_num = get_month_number( ).
    rv_short_month = get_short_month_name_by_number( lv_month_num ).
  ENDMETHOD.


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


  METHOD get_short_year.
    rv_short_year = gv_date+2(2).
  ENDMETHOD.


  METHOD get_year.
    rv_year = gv_date+0(4).
  ENDMETHOD.


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


  METHOD minus.
    date_calculation( iv_days   = iv_days
                      iv_months = iv_months
                      iv_years  = iv_years
                      iv_sign   = '-' ).
  ENDMETHOD.


  METHOD obj_diff.
    DATA:
      lv_dats1 TYPE dats,
      lv_dats2 TYPE dats.

    lv_dats1 = iv_dat1->get_date( ).
    lv_dats2 = iv_dat2->get_date( ).

    rv_diff = lv_dats1 - lv_dats2.
  ENDMETHOD.


  METHOD plus.
    date_calculation( iv_days   = iv_days
                      iv_months = iv_months
                      iv_years  = iv_years
                      iv_sign   = '+' ).
  ENDMETHOD.


  METHOD set_date.
    gv_date = iv_date.
  ENDMETHOD.


  METHOD set_day.
    gv_date+6(2) = iv_day.
  ENDMETHOD.


  METHOD set_language.
    gv_language = iv_language.
  ENDMETHOD.


  METHOD set_month_number.
    gv_date+4(2) = iv_month_number.
  ENDMETHOD.


  METHOD set_year.
    gv_date+0(4) = iv_year.
  ENDMETHOD.


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
