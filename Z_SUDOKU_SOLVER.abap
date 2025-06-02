*&---------------------------------------------------------------------*
*& Report Z_SUDOKU_SOLVER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE z_sudoku_solver_top.

************************************************************************
START-OF-SELECTION.

  DATA: lv_source TYPE fieldname.
  DATA: ls_grid TYPE zcl_sudoku_solver=>ty_grid.

  DO 9 TIMES.
    ls_grid-row = sy-index.
    DO 9 TIMES.
      ls_grid-col = sy-index.
      lv_source = 'CELL_' && ls_grid-row && ls_grid-col.
      ASSIGN (lv_source)
             TO FIELD-SYMBOL(<lv_source>).
      IF sy-subrc EQ 0.
        ls_grid-value = <lv_source>.
        INSERT ls_grid
               INTO TABLE gt_grid.
      ENDIF.
    ENDDO.
  ENDDO.

  go_sudoku_solver = NEW #( gt_grid ).

  go_sudoku_solver->solve( ).

  gt_grid = go_sudoku_solver->get_result( ).

  NEW-LINE.
  LOOP AT gt_grid
       ASSIGNING FIELD-SYMBOL(<ls_grid>).
    IF <ls_grid>-col EQ 1.
      NEW-LINE.
    ENDIF.
    WRITE: '|', <ls_grid>-value.
    IF <ls_grid>-col EQ 9.
      WRITE '|'.
      WRITE / '|---|---|---|---|---|---|---|---|---|'.
    ENDIF.
  ENDLOOP.
