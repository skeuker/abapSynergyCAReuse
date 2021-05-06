*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01.05.2021 at 23:42:51
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCA_V_DEBUG.....................................*
TABLES: ZCA_V_DEBUG, *ZCA_V_DEBUG. "view work areas
CONTROLS: TCTRL_ZCA_V_DEBUG
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZCA_V_DEBUG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZCA_V_DEBUG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZCA_V_DEBUG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZCA_V_DEBUG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCA_V_DEBUG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZCA_V_DEBUG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZCA_V_DEBUG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCA_V_DEBUG_TOTAL.

*.........table declarations:.................................*
TABLES: ZCA_DEBUG                      .
