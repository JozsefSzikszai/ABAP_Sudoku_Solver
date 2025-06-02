*&---------------------------------------------------------------------*
*& Include Z_SUDOKU_SOLVER_TOP                      - Report Z_SUDOKU_SOLVER
*&---------------------------------------------------------------------*
REPORT z_sudoku_solver.

TYPES: ty_cell TYPE n LENGTH 1.

DATA: go_sudoku_solver TYPE REF TO zcl_sudoku_solver.

DATA: gt_grid TYPE zcl_sudoku_solver=>tt_grid.

************************************************************************
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_11 TYPE ty_cell,
              cell_12 TYPE ty_cell,
              cell_13 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_14 TYPE ty_cell,
              cell_15 TYPE ty_cell,
              cell_16 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_17 TYPE ty_cell,
              cell_18 TYPE ty_cell,
              cell_19 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_21 TYPE ty_cell,
              cell_22 TYPE ty_cell,
              cell_23 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_24 TYPE ty_cell,
              cell_25 TYPE ty_cell,
              cell_26 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_27 TYPE ty_cell,
              cell_28 TYPE ty_cell,
              cell_29 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_31 TYPE ty_cell,
              cell_32 TYPE ty_cell,
              cell_33 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_34 TYPE ty_cell,
              cell_35 TYPE ty_cell,
              cell_36 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_37 TYPE ty_cell,
              cell_38 TYPE ty_cell,
              cell_39 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_41 TYPE ty_cell,
              cell_42 TYPE ty_cell,
              cell_43 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_44 TYPE ty_cell,
              cell_45 TYPE ty_cell,
              cell_46 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_47 TYPE ty_cell,
              cell_48 TYPE ty_cell,
              cell_49 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_51 TYPE ty_cell,
              cell_52 TYPE ty_cell,
              cell_53 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_54 TYPE ty_cell,
              cell_55 TYPE ty_cell,
              cell_56 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_57 TYPE ty_cell,
              cell_58 TYPE ty_cell,
              cell_59 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_61 TYPE ty_cell,
              cell_62 TYPE ty_cell,
              cell_63 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_64 TYPE ty_cell,
              cell_65 TYPE ty_cell,
              cell_66 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_67 TYPE ty_cell,
              cell_68 TYPE ty_cell,
              cell_69 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_71 TYPE ty_cell,
              cell_72 TYPE ty_cell,
              cell_73 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_74 TYPE ty_cell,
              cell_75 TYPE ty_cell,
              cell_76 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_77 TYPE ty_cell,
              cell_78 TYPE ty_cell,
              cell_79 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_81 TYPE ty_cell,
              cell_82 TYPE ty_cell,
              cell_83 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_84 TYPE ty_cell,
              cell_85 TYPE ty_cell,
              cell_86 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_87 TYPE ty_cell,
              cell_88 TYPE ty_cell,
              cell_89 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 3.
  PARAMETERS: cell_91 TYPE ty_cell,
              cell_92 TYPE ty_cell,
              cell_93 TYPE ty_cell.
  SELECTION-SCREEN POSITION 11.
  PARAMETERS: cell_94 TYPE ty_cell,
              cell_95 TYPE ty_cell,
              cell_96 TYPE ty_cell.
  SELECTION-SCREEN POSITION 19.
  PARAMETERS: cell_97 TYPE ty_cell,
              cell_98 TYPE ty_cell,
              cell_99 TYPE ty_cell.
SELECTION-SCREEN END OF LINE.
