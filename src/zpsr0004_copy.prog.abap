*&---------------------------------------------------------------------*
*& Report ZPSR0004
*&---------------------------------------------------------------------*
*&Purpose       : Get data from alv tcode CN43N
*&System Analyst: R. Dwita Nur Aini
*&Abaper        : Christina Diana Aryanti
*&Date          : 17.07.2023
*&---------------------------------------------------------------------*
REPORT zpsr0004_copy.
TABLES: varid.
TYPES:
    BEGIN OF ty_line,
      line TYPE c LENGTH 262143,
    END OF ty_line,
    ty_t_line TYPE STANDARD TABLE OF ty_line.

DATA: gt_line TYPE ty_t_line,
      gs_line TYPE ty_line,
      gt_line_done  TYPE ty_t_line,
      gs_line_done  TYPE ty_line.

DATA : idetails TYPE abap_compdescr_tab,
       xdetails TYPE abap_compdescr.
DATA : ref_table_des TYPE REF TO cl_abap_structdescr.
DATA: lv_concat_string TYPE string,
      lv_line_string   TYPE ty_line,
      lv_line          TYPE string,
      li_line          TYPE int8,
      lv_process       TYPE string,
      li_process       TYPE int8,
      lv_count         TYPE string,
      lv_from          TYPE string,
      li_from          TYPE int8,
      lv_to            TYPE string,
      li_to            TYPE int8,
      lt_line_done     TYPE ty_t_line,
      ls_line_done     TYPE ty_line,
      lv_search(100)   TYPE c,
      lv_seq           TYPE string.

CONSTANTS:
  gc_data_sep         TYPE char1          VALUE ';',
  gc_fname_sep        TYPE char1          VALUE '.',
  gc_undscore_sep     TYPE char1          VALUE '_',
  gc_file_ext         TYPE string         VALUE 'TXT'.

"DEFINE TABLE PENAMPUNG UNTUK TABLE ALV DARI REPORT YANG AKAN KITA AMBIL, PASTIKAN TYPE DATA NYA SAMA DENGAN
DATA : t_data TYPE STANDARD TABLE OF prpsi WITH HEADER LINE.
DATA : t_selection TYPE STANDARD TABLE OF rsparams WITH HEADER LINE.

*----------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
SELECTION-SCREEN:BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-000.
  PARAMETERS : p_prog TYPE progname DEFAULT 'RPSISPE000' OBLIGATORY,
               p_var TYPE varid-variant,"RALDB_VARI.
               p_path TYPE string DEFAULT '/usr/sap/tmp/talend/'.

SELECTION-SCREEN: END OF BLOCK blk1.

"CALL PROGRAM YANG TABLE ALV NYA MAU KITA AMBIL, SETELAH FM INI JALAN, MAKA NANTI T_DATA ITU AKAN BERISI DATA DARI PROGRAM YANG KITA PANGGIL TADI
CALL FUNCTION 'ZFM_GET_TABLE_ALV_FROM_REPORT'
  EXPORTING
    i_pname       = p_prog
    i_submit_mode = '2'
    i_variant     = p_var
  TABLES
    t_data        = t_data
*    t_rsparams    = t_selection
    .

DESCRIBE TABLE t_data LINES lv_line.
li_line = lv_line.
lv_process = 5000.
li_process = lv_process.
IF lv_line IS NOT INITIAL.
   lv_count = lv_line / lv_process.
ENDIF.
IF lv_count < 0.
  lv_count = 1.
ELSE.
  lv_count = round( val = lv_count dec = 0 ).
  lv_count = lv_count + 1.
ENDIF.

IF li_line > 0.
  lv_from = 1.
  IF li_line > li_process.
    lv_to = lv_process.
  ELSE.
    lv_to = lv_line.
  ENDIF.
  li_from = lv_from.
  li_to = lv_to.
ELSE.
  lv_count = 0.
ENDIF.

CONCATENATE 'ZCN43N_' p_var '%' INTO lv_search.
select max( seq ) INTO lv_seq
  FROM zre_log_talend
  WHERE filename LIKE lv_search
  AND datum = sy-datum.

lv_seq = lv_seq + 1.

DO lv_count TIMES.
  IF li_from >= li_to.
    EXIT.
  ENDIF.
  PERFORM f_populate_header CHANGING gt_line.
  LOOP AT t_data FROM li_from TO li_to.
    PERFORM f_populate_item   USING t_data
                              CHANGING gt_line
                                       lv_line_string.

   "Concate data to string
    CONCATENATE lv_concat_string
                lv_line_string
           INTO lv_concat_string.
  ENDLOOP.
  lv_to = li_to.
  PERFORM f_transfer_data USING gt_line
                                lv_seq
                                lv_line
                                lv_process
                                lv_to
                                lv_concat_string
                       CHANGING lt_line_done.

  li_from = li_to + 1.
  li_to   = ( li_from + li_process ) - 1.
  IF li_line < li_to.
    li_to = li_line.
  ENDIF.
ENDDO.
IF lv_count > 0.
  PERFORM f_transfer_flag_file USING lt_line_done
                                       lv_line
                                       lv_process
                                       lv_seq.
ENDIF.

*
*&---------------------------------------------------------------------*
*& Form f_populate_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_LINE
*&---------------------------------------------------------------------*
FORM f_populate_header  CHANGING p_gt_line  TYPE ty_t_line.
  DATA:
    ls_line   TYPE ty_line.

*  ref_table_des ?= cl_abap_typedescr=>describe_by_name( t_data ).
*  idetails[] = ref_table_des->components[].
*  LOOP AT idetails INTO xdetails.
*    WRITE: /,xdetails-name.
*  ENDLOOP.
  CONCATENATE 'Project definition'
              'WBS element'
              'Person responsible'
              'Name'
              'Level'
              'CA'
              'No. of resp. person'
              'Basic finish date'
              'Basic start date'
              'Deletion Indicator'
              'Status'
              'Company code'
              'SiteId'
              'Customer'
              'Profit center'
              'Resp. cost center'
              'Desc. of resp. CC'
              INTO ls_line-line
              SEPARATED BY gc_data_sep.

  APPEND ls_line-line TO p_gt_line.
  CLEAR ls_line-line.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_populate_item
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_DATA
*&      <-- P_GT_LINE
*&      <-- LV_LINE_STRING
*&---------------------------------------------------------------------*
FORM f_populate_item  USING p_t_data TYPE prpsi
                      CHANGING p_gt_line TYPE ty_t_line
                               p_lv_line_string TYPE ty_line.

DATA:
  ls_line   TYPE ty_line,
  lv_pspid(100) TYPE c,
  lv_posid(200) TYPE c.


DEFINE m_build_line.
  CONCATENATE ls_line-line
              &1
  INTO  ls_line-line
  SEPARATED BY gc_data_sep.
END-OF-DEFINITION.
CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
  EXPORTING
    input        = p_t_data-pspid
 IMPORTING
   output        = lv_pspid.

CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
  EXPORTING
    input        = p_t_data-posid
 IMPORTING
   output        = lv_posid.

ls_line-line = lv_pspid.
m_build_line lv_posid.
m_build_line p_t_data-verna.
REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN p_t_data-post1 WITH | |. "Add: Christina, 22.09.2023 AM001-4045 TR:ECDK918845
m_build_line p_t_data-post1.
m_build_line p_t_data-stufe.
m_build_line p_t_data-pkokr.
m_build_line p_t_data-vernr.
m_build_line p_t_data-pende.
m_build_line p_t_data-pstrt.
m_build_line p_t_data-loekz.
m_build_line p_t_data-statxt.
m_build_line p_t_data-pbukr.
m_build_line p_t_data-usr00. "USR00 siteid
m_build_line p_t_data-usr02. "customer
m_build_line p_t_data-prctr. "profit center
m_build_line p_t_data-fkstl. "resp cost center
m_build_line p_t_data-fktxt. "desc of resp cc
MOVE ls_line TO p_lv_line_string.
APPEND ls_line TO p_gt_line.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_transfer_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_LINE
*&      --> LV_LINE
*&      --> LV_PROCESS
*&      --> LV_TO
*&      --> LV_CONCAT_STRING
*&      <-- LT_LINE_DONE
*&---------------------------------------------------------------------*
FORM f_transfer_data  USING    p_gt_line          TYPE ty_t_line
                               p_lv_seq           TYPE string
                               p_lv_line          TYPE string
                               p_lv_process       TYPE string
                               p_lv_to            TYPE string
                               p_lv_concat_string TYPE string
                      CHANGING p_lt_line_done     TYPE ty_t_line.
  DATA:
      lv_filename      TYPE text100,
      lv_path_filename TYPE string,
      lv_seq TYPE string,
      lv_search(100) TYPE c.

  DATA:
    lt_zre_log_talend TYPE STANDARD TABLE OF zre_log_talend,
    ls_zre_log_talend TYPE zre_log_talend,
    lv_totline TYPE string,
    li_totline TYPE int8.



  CONCATENATE 'ZCN43N_'
                p_var
                gc_undscore_sep
                sy-datum
                gc_undscore_sep
                p_lv_seq
                gc_undscore_sep
                p_lv_to
                gc_fname_sep
                gc_file_ext
  INTO lv_filename.
  DESCRIBE TABLE p_gt_line LINES li_totline.
  lv_totline = li_totline - 1.

  CONDENSE lv_filename NO-GAPS.
  CONCATENATE p_path lv_filename INTO lv_path_filename.
  CONDENSE lv_path_filename NO-GAPS.

  TRANSLATE lv_path_filename TO LOWER CASE.
  OPEN DATASET lv_path_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT WITH SMART LINEFEED.

  IF sy-subrc EQ 0.
    LOOP AT p_gt_line INTO gs_line.
      TRANSFER gs_line TO lv_path_filename.
    ENDLOOP.
    "Fill table log when success
    ls_zre_log_talend-filename    = lv_filename.
    ls_zre_log_talend-seq         = p_lv_seq.
    ls_zre_log_talend-total_lines = lv_totline.
    ls_zre_log_talend-devider     = p_lv_process.
    ls_zre_log_talend-datum       = sy-datum.
    ls_zre_log_talend-uzeit       = sy-uzeit.
    ls_zre_log_talend-msgty       = 'S'.
    ls_zre_log_talend-natxt       = 'File transfered successfully'.
    APPEND ls_zre_log_talend TO lt_zre_log_talend.
    CLEAR ls_zre_log_talend.
  ELSE.
    "Fill table log when error
    ls_zre_log_talend-filename    = lv_filename.
    ls_zre_log_talend-seq         = p_lv_seq.
    ls_zre_log_talend-total_lines = lv_totline.
    ls_zre_log_talend-devider     = p_lv_process.
    ls_zre_log_talend-datum       = sy-datum.
    ls_zre_log_talend-uzeit       = sy-uzeit.
    ls_zre_log_talend-msgty       = 'E'.
    ls_zre_log_talend-natxt       = 'File transfer not successfully'.
    APPEND ls_zre_log_talend TO lt_zre_log_talend.
    CLEAR ls_zre_log_talend.
  ENDIF.

  "Commit to table
  IF lt_zre_log_talend[] IS NOT INITIAL.
    INSERT zre_log_talend FROM TABLE lt_zre_log_talend.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  CLOSE DATASET lv_path_filename.

  PERFORM f_generating_hash_file USING lv_filename
                                       p_lv_concat_string
                              CHANGING p_lt_line_done.

  FREE:
    p_gt_line,
    lv_filename,
    lv_path_filename.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_generating_hash_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_FILENAME
*&      --> P_LV_CONCAT_STRING
*&      <-- P_LT_LINE_DONE
*&---------------------------------------------------------------------*
FORM f_generating_hash_file  USING    p_lv_filename       TYPE text100
                                      p_lv_concat_string  TYPE string
                             CHANGING p_lt_line_done      TYPE ty_t_line.

DATA:
  lv_length TYPE i,
  lv_hash   TYPE hash160,
  ls_line_done TYPE ty_line.

  lv_length = strlen( p_lv_concat_string ).

  CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
    EXPORTING
      alg            = 'MD5'
      data           = p_lv_concat_string
      length         = lv_length
    IMPORTING
      hash           = lv_hash
    EXCEPTIONS
      unknown_alg    = 1
      param_error    = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

  "Concate filename dan hash
  CONCATENATE p_lv_filename
              gc_data_sep
              lv_hash
         INTO ls_line_done.
  APPEND ls_line_done TO p_lt_line_done.
  CLEAR: ls_line_done,
         lv_length,
         lv_hash.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_transfer_flag_file
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_LINE_DONE
*&      --> LV_SEQ
*&      --> LV_LINE
*&      --> LV_PROCESS
*&---------------------------------------------------------------------*
FORM f_transfer_flag_file  USING    p_lt_line_done TYPE ty_t_line
                                    p_lv_line      TYPE string
                                    p_lv_process   TYPE string
                                    p_lv_seq       TYPE string.

 DATA:
    lv_filename      TYPE text100,
    lv_path_filename TYPE string,
    lv_seq           TYPE string,
    lv_search(100)   TYPE c.

  CLEAR: lv_filename,
         gs_line.



 CONCATENATE 'ZCN43N_'
                p_var
                gc_undscore_sep
                sy-datum
                gc_undscore_sep
                p_lv_seq
                gc_undscore_sep
                'DONE'
                gc_fname_sep
                gc_file_ext
  INTO lv_filename.

  CONDENSE lv_filename NO-GAPS.
  CONCATENATE p_path lv_filename INTO lv_path_filename.
  CONDENSE lv_path_filename NO-GAPS.
  TRANSLATE lv_path_filename TO LOWER CASE.

  OPEN DATASET lv_path_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT WITH SMART LINEFEED.
  IF sy-subrc EQ 0.
    LOOP AT p_lt_line_done INTO gs_line_done.
      TRANSFER gs_line_done TO lv_path_filename.
    ENDLOOP.
  ENDIF.
  CLOSE DATASET lv_path_filename.

  "Add: Christina for testing

  "Add: Christina for testing 1

ENDFORM.
