CLASS zcl_sudoku_solver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_cell TYPE n LENGTH 1 .
    TYPES:
      BEGIN OF ty_possible,
        value TYPE ty_cell,
      END OF ty_possible .
    TYPES:
      tt_possible TYPE SORTED TABLE OF ty_possible
                              WITH UNIQUE KEY value .
    TYPES:
      BEGIN OF ty_grid,
        row   TYPE i,
        col   TYPE i,
        value TYPE ty_cell,
      END OF ty_grid .
    TYPES:
      tt_grid TYPE SORTED TABLE OF ty_grid
                          WITH UNIQUE KEY row
                                          col .

    METHODS constructor
      IMPORTING
        !it_grid TYPE tt_grid .
    METHODS get_result
      RETURNING
        VALUE(rt_grid) TYPE tt_grid .
    METHODS solve .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_view,
        item_1 TYPE tt_possible,
        item_2 TYPE tt_possible,
        item_3 TYPE tt_possible,
        item_4 TYPE tt_possible,
        item_5 TYPE tt_possible,
        item_6 TYPE tt_possible,
        item_7 TYPE tt_possible,
        item_8 TYPE tt_possible,
        item_9 TYPE tt_possible,
      END OF ty_view .

    DATA ms_possible_block TYPE ty_view .
    DATA ms_possible_column TYPE ty_view .
    DATA ms_possible_row TYPE ty_view .
    DATA mt_grid TYPE tt_grid .
    DATA mt_possible_default TYPE tt_possible .
    DATA mv_missing TYPE i .

    METHODS add_possible_values
      IMPORTING
        !iv_index      TYPE syindex
        !is_line       TYPE ty_view
      RETURNING
        VALUE(rs_line) TYPE ty_view .
    METHODS determine_block_number
      IMPORTING
        !iv_row         TYPE i
        !iv_col         TYPE i
      RETURNING
        VALUE(rv_block) TYPE i .
    METHODS get_possible
      IMPORTING
        !iv_index          TYPE syindex
        !is_line           TYPE ty_view
      RETURNING
        VALUE(rt_possible) TYPE tt_possible .
    METHODS initialize_grid_possible .
    METHODS initialize_possible_all .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SUDOKU_SOLVER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SUDOKU_SOLVER->ADD_POSSIBLE_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        SYINDEX
* | [--->] IS_LINE                        TYPE        TY_VIEW
* | [<-()] RS_LINE                        TYPE        TY_VIEW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_possible_values.

    rs_line = is_line.
    ASSIGN COMPONENT sy-index
       OF STRUCTURE rs_line
       TO FIELD-SYMBOL(<lt_default>).
    IF sy-subrc EQ 0.
      <lt_default> = mt_possible_default.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SUDOKU_SOLVER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_GRID                        TYPE        TT_GRID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    mt_grid = it_grid.

    initialize_possible_all( ).
    initialize_grid_possible( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SUDOKU_SOLVER->DETERMINE_BLOCK_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROW                         TYPE        I
* | [--->] IV_COL                         TYPE        I
* | [<-()] RV_BLOCK                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD determine_block_number.

    rv_block = COND #( WHEN ( iv_row BETWEEN 1 AND 3 ) AND ( iv_col BETWEEN 1 AND 3 ) THEN 1
                       WHEN ( iv_row BETWEEN 1 AND 3 ) AND ( iv_col BETWEEN 4 AND 6 ) THEN 2
                       WHEN ( iv_row BETWEEN 1 AND 3 ) AND ( iv_col BETWEEN 7 AND 9 ) THEN 3
                       WHEN ( iv_row BETWEEN 4 AND 6 ) AND ( iv_col BETWEEN 1 AND 3 ) THEN 4
                       WHEN ( iv_row BETWEEN 4 AND 6 ) AND ( iv_col BETWEEN 4 AND 6 ) THEN 5
                       WHEN ( iv_row BETWEEN 4 AND 6 ) AND ( iv_col BETWEEN 7 AND 9 ) THEN 6
                       WHEN ( iv_row BETWEEN 7 AND 9 ) AND ( iv_col BETWEEN 1 AND 3 ) THEN 7
                       WHEN ( iv_row BETWEEN 7 AND 9 ) AND ( iv_col BETWEEN 4 AND 6 ) THEN 8
                       WHEN ( iv_row BETWEEN 7 AND 9 ) AND ( iv_col BETWEEN 7 AND 9 ) THEN 9 ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SUDOKU_SOLVER->GET_POSSIBLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        SYINDEX
* | [--->] IS_LINE                        TYPE        TY_VIEW
* | [<-()] RT_POSSIBLE                    TYPE        TT_POSSIBLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_possible.

    ASSIGN COMPONENT iv_index
           OF STRUCTURE is_line
           TO FIELD-SYMBOL(<lt_possible>).
    IF sy-subrc EQ 0.
      rt_possible = <lt_possible>.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SUDOKU_SOLVER->GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_GRID                        TYPE        TT_GRID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_result.

    rt_grid = mt_grid.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SUDOKU_SOLVER->INITIALIZE_GRID_POSSIBLE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_grid_possible.

    FIELD-SYMBOLS: <lt_possible> TYPE tt_possible.

************************************************************************
    LOOP AT mt_grid
         ASSIGNING FIELD-SYMBOL(<ls_grid>).
      IF <ls_grid>-value NE 0.
        SUBTRACT 1 FROM mv_missing.
        ASSIGN COMPONENT <ls_grid>-row
               OF STRUCTURE ms_possible_row
               TO <lt_possible>.
        IF sy-subrc EQ 0.
          DELETE <lt_possible>
                 WHERE value EQ <ls_grid>-value.
        ENDIF.
        ASSIGN COMPONENT <ls_grid>-col
               OF STRUCTURE ms_possible_column
               TO <lt_possible>.
        IF sy-subrc EQ 0.
          DELETE <lt_possible>
                 WHERE value EQ <ls_grid>-value.
        ENDIF.
        DATA(lv_block) = determine_block_number( iv_row = <ls_grid>-row
                                                 iv_col = <ls_grid>-col ).
        ASSIGN COMPONENT lv_block
               OF STRUCTURE ms_possible_block
               TO <lt_possible>.
        IF sy-subrc EQ 0.
          DELETE <lt_possible>
                 WHERE value EQ <ls_grid>-value.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ADD 81 TO mv_missing.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SUDOKU_SOLVER->INITIALIZE_POSSIBLE_ALL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize_possible_all.

    mt_possible_default = VALUE #( FOR i = 1 THEN i + 1 WHILE i < 10
                                 ( value = i ) ).

    DO 9 TIMES.
      ms_possible_row = add_possible_values( iv_index = sy-index
                                             is_line  = ms_possible_row ).
      ms_possible_column = add_possible_values( iv_index = sy-index
                                                is_line  = ms_possible_column ).
      ms_possible_block = add_possible_values( iv_index = sy-index
                                               is_line  = ms_possible_block ).
    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SUDOKU_SOLVER->SOLVE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD solve.

    DATA: lt_possible_all TYPE tt_possible.

    FIELD-SYMBOLS: <lt_possible_row>    TYPE tt_possible,
                   <lt_possible_column> TYPE tt_possible,
                   <lt_possible_block>  TYPE tt_possible.

    DATA: lv_missing TYPE i.

************************************************************************
    WHILE mv_missing GT 0.
      CLEAR: lv_missing.
      LOOP AT mt_grid
           ASSIGNING FIELD-SYMBOL(<ls_grid>)
           WHERE value EQ 0.
        ASSIGN COMPONENT <ls_grid>-row
               OF STRUCTURE ms_possible_row
               TO <lt_possible_row>.
        ASSIGN COMPONENT <ls_grid>-col
               OF STRUCTURE ms_possible_column
               TO <lt_possible_column>.
        DATA(lv_block) = determine_block_number( iv_row = <ls_grid>-row
                                                 iv_col = <ls_grid>-col ).
        ASSIGN COMPONENT lv_block
               OF STRUCTURE ms_possible_block
               TO <lt_possible_block>.
        IF <lt_possible_row>    IS ASSIGNED AND
           <lt_possible_column> IS ASSIGNED AND
           <lt_possible_block>  IS ASSIGNED.
          CLEAR: lt_possible_all.
          DO 9 TIMES.
            IF line_exists( <lt_possible_row>[ value = sy-index ] ) AND
               line_exists( <lt_possible_column>[ value = sy-index ] ) AND
               line_exists( <lt_possible_block>[ value = sy-index ] ).
              DATA(ls_possible_all) = VALUE ty_possible( value = sy-index ).
              INSERT ls_possible_all
                     INTO TABLE lt_possible_all.
            ENDIF.
          ENDDO.
          IF lines( lt_possible_all ) EQ 1.
            <ls_grid>-value = VALUE #( lt_possible_all[ 1 ]-value ).
            DELETE <lt_possible_row>
                   WHERE value EQ <ls_grid>-value.
            DELETE <lt_possible_column>
                   WHERE value EQ <ls_grid>-value.
            DELETE <lt_possible_block>
                   WHERE value EQ <ls_grid>-value.
          ELSE.
            ADD 1 TO lv_missing.
          ENDIF.

        ENDIF.
        UNASSIGN: <lt_possible_row>,
                  <lt_possible_column>,
                  <lt_possible_block>.
      ENDLOOP.
      mv_missing = lv_missing.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
