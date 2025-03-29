;===================================================================================================
;������� ������������ ������� V-0.1
;===================================================================================================
(defun c:ukl()
	(progn
	  	(setq C_WCS (getvar "WORLDUCS"))		;������� ������� ��������� 1 ��� 0 ���
		(setq S_text "Standard")			;����� ������
		(setq H_text 0.01)				;������ ������
		(setq C_layr 7)					;���� ����
		(setq N_layr "�����")				;��� ����
		(setq L_list nil)				;������ ����
		(setq S_list nil)				;������ ������ ������
		(setq Grad "")					;�����
		(setq S_Grad 1)					;����� ������������ ������
	  	(setq S_prHXY 1)				;���������� ��� ������� ������
		(setq S_Arrow 2)				;����� ���������
		(Dialog)					;�������� ����������� ����
	); progn
);defun c:ukl

;===================================================================================================
; �������� ��������� �� ��� � ��� � �������
;===================================================================================================

;��������� ���������� ������� ������� ���������
(defun GetUCSParams (/ origin xdir ydir angx angy)
  (setq origin (getvar "UCSORG"))
  (setq xdir (getvar "UCSXDIR"))
  (setq ydir (getvar "UCSYDIR"))
  
  ;��������� ���� �������� ����
  (setq angx (atan (cadr xdir) (car xdir))) ; ���� ��� X ���
  (setq angy (atan (cadr ydir) (car ydir))) ; ���� ��� Y ���
  
  (list origin angx angy)
)

;�������� �� ��� � ���
(defun UCS_to_WCS (point / params)
  
  (setq params (GetUCSParams))
  ;(UCS_to_WCS point (nth 0 params) (nth 1 params) (nth 2 params))
  (setq x (car point))
  (setq y (cadr point))
  (setq z (caddr point))
  ;���������� ��������� � ���
  (list
    (+ (car (nth 0 params)) (* x (cos (nth 1 params))) (* y (cos (nth 2 params))))
    (+ (cadr (nth 0 params)) (* x (sin (nth 1 params))) (* y (sin (nth 2 params))))
    (+ (caddr (nth 0 params)) z)
  )
)

;�������� �� ��� � ���
(defun WCS_to_UCS (point / params)
  (setq params (GetUCSParams))
  ;(WCS_to_UCS point (nth 0 params) (nth 1 params) (nth 2 params))
  (setq dx (- (car point) (car (nth 0 params))))
  (setq dy (- (cadr point) (cadr (nth 0 params))))
  (setq dz (- (caddr point) (caddr (nth 0 params))))
  
  (setq sinx (sin (nth 1 params)))
  (setq cosx (cos (nth 1 params)))
  (setq siny (sin (nth 2 params)))
  (setq cosy (cos (nth 2 params)))
  
  ;������������ ������� ��������������
  (setq det (- (* cosx siny) (* sinx cosy)))
  
  (if (/= det 0.0)
    (progn
      ;��������� ���������� � ���
      (setq xp (/ (- (* dy cosy) (* dx siny)) det))
      (setq yp (/ (- (* dx cosx) (* dy sinx)) det))
      (list xp yp dz)
    )
    ;���� ������������ ����� ����, ��� ����������
    (progn
      (princ "\n������: ��� X � Y �����������")
      nil
    )
  )
)

;===================================================================================================
;�������� ����� ��� ������� ������
;===================================================================================================
(defun Enter_Point()
	(setq P1 (getpoint "����� 1: "))			;�������� ������������� ����� �1
	(setq P2 (getpoint "����� 2: "))			;�������� ������������� ����� �2
  	(cond
	  ((= S_prHXY 1)
	   	(setq Dist (distance P1 P2))			;���������� ����� ������� �1 � �2
		(setq Elev (- (caddr P1) (caddr P2))))		;���������� P1 - P2 = Elev �� �
	  ((= S_prHXY 2)
	   	(setq Dist (distance P1 P2))			;���������� ����� ������� �1 � �2
		(setq Elev (- (car P1) (car P2))))		;���������� P1 - P2 = Elev �� X
	  ((= S_prHXY 3)
	   	(setq Dist (distance P1 P2))			;���������� ����� ������� �1 � �2
		(setq Elev (- (cadr P1) (cadr P2))))		;���������� P1 - P2 = Elev �� Y
	  ((= S_prHXY 4)
	   	(setq Dist (distance P1 P2))			;���������� ����� ������� �1 � �
		(setq PE1 (list (car P1) (cadr P1) 0.0))	;��������� � ����� P1
	   	(setq PE2 (list (car P2) (cadr P2) 0.0))	;��������� � ����� P2
		(setq Elev (distance PE1 PE2)))			;���������� P1 - P2 = Elev �� XY
	  );S_prHXY	
	
	(Calc_Grad S_Grad)					;������ ������ �� ����� 
	(Print_Grad)						;����� �����������
	(Enter_Point)						;�������� ������
);Enter_Point

;===================================================================================================
;����� ����������� �������� ������ � �������
;FirP - ����� ��� ������ ��������� ��� ������� ������ � ����� ������ (������)
;SecP - ����� ��� ���������� ���� (�������)
;TextAngle - ���� ����� ������� ��� ������ 
;DrawAngle - ���� ��� ��������� ����� ������
;ForLP3 - �������� ���� ��� ����� LP3 ��� ����������� ����� ������
;===================================================================================================
(defun Print_Grad()
	;����������� ����� �������� ������ � ����������� �������� �������
	;� ���������� �� ���������� (Elev) � �������� ��������� (dX, dY)
	;������ ����� ���� ������ ����
  	(if (= C_WCS 0)				;�������� ��������� �� ��� � ���
	  	(progn (WCS_to_UCS P1))
	  	(progn (WCS_to_UCS P2))
	  )
	(if (< Elev 0)
		(progn (setq FirP P1) (setq SecP P2))
		(progn (setq FirP P2) (setq SecP P1))
	);if
	(setq dX (-(car SecP) (car FirP))) 	; �������� ��������� ����� �2 � �1 �� �
	(setq dY (-(cadr SecP) (cadr FirP)))	; �������� ��������� ����� �2 � �1 �� Y

	(progn
		;������ ��������
		(if (and (>= dX 0) (>= dY 0))
			(progn
				(print (strcat "����� ��������"))
				(setq TextAngle (angle FirP SecP))
				(setq DrawAngle (- TextAngle PI))
				(setq ForLP3 (- DrawAngle (/ PI 2)))
				(setq PosLP3 "N")
			);progn
		);if ������ ��������
		;������ ��������
		(if (and (>= dX 0) (< dY 0))
			(progn
				(print (strcat "������ ��������"))
				(setq TextAngle (angle FirP SecP))
				(setq DrawAngle (- TextAngle PI))
				(setq ForLP3 (- DrawAngle (/ PI 2)))
				(setq PosLP3 "N")
			);progn
		);if ������ ��������
		;������ ��������
		(if (and (< dX 0) (< dY 0))
			(progn
				(print (strcat "������ ��������"))
				(setq TextAngle (angle SecP FirP))
				(setq DrawAngle (+ TextAngle PI))
				(setq ForLP3 (- DrawAngle (/ PI 2)))
				(setq PosLP3 "R")
			);progn
		);if ������ ��������
		;��������� ��������
		(if (and (< dX 0) (>= dY 0))
			(progn
				(print (strcat "��������� ��������"))
				(setq TextAngle (angle SecP FirP))
				(setq DrawAngle (+ TextAngle PI))
				(setq ForLP3 (- DrawAngle (/ PI 2)))
				(setq PosLP3 "R")
			);progn
		);if ��������� ��������
	);progn
;===================================================================================================
;������� ������
;===================================================================================================
	(setq XYZ (getpoint "����� ������� "))
  	(if (= C_WCS 0)				
		(progn (setq XYZ (UCS_to_WCS XYZ))) ;�������� ��������� �� ��� � ���
	)
	;DXF ����� ������� ������
  	(entmake (list
						(cons 0 "TEXT")
						(cons 8 N_layr)
						(cons 62 C_layr)
						(cons 10 XYZ)
						(cons 40 H_text)
						(cons 50 TextAngle)
						(cons 7 S_text)
						(cons 1 Grad)
						(cons 11 XYZ);���������� ����� ������������ ������
						(cons 72 1);������������ �� ����������� 
						(cons 73 1);������������ �� ���������
	));entmake TEXT
;===================================================================================================
;���� ������� ������ ��� ��������� ���������
;���������� ������� ������
;��������� ������� �������� ���������� �����
;===================================================================================================
	(if(= S_Arrow 1)
		(progn
			(setq PosText (cdr (assoc 11 (entget (entlast)))))
		  	;(if (= C_WCS 0)				
	  		;	(progn (setq PosText (UCS_to_WCS PosText))) ;�������� ��������� �� ��� � ���
	 		;)
			;����� ������� �� ������ ������
			(setq DrawDist (/ H_text 3))
			;������ ��������� ����� ������
		  ;        LP3___								
		  ;               ---___				- ������ ��������� ������� ������
		  ;	       LP1----------LP2			
			(setq LP1 (polar PosText DrawAngle DrawDist))
			(setq LP2 (polar LP1 DrawAngle (* H_text 2)))
			(if (= PosLP3 "N")
				(progn (setq LP3 (polar LP1 ForLP3 H_text)) (setq FP1 LP1 FP2 LP2 FP3 LP3))
				(progn (setq LP3 (polar LP2 ForLP3 H_text)) (setq FP1 LP2 FP2 LP1 FP3 LP3))
			);if PosLP3
			;������� ����� ������
			(entmake (list
								(cons 0 "LWPOLYLINE")
								(cons 8 N_layr)
								(cons 62 C_layr)
								(cons 100 "AcDbEntity")
								(cons 100 "AcDbPolyline")
								(cons 90 3)
								(list 10 (car FP1) (cadr FP1))
								(list 10 (car FP2) (cadr FP2))
								(list 10 (car FP3) (cadr FP3))						
			));entmake LWPOLYLINE
		);progn
	);if
	(if(= S_Arrow 2)
		(progn
			(setq PosText (cdr (assoc 11 (entget (entlast)))))
		  	;(if (= C_WCS 0)				
	  		;	(progn (setq PosText (UCS_to_WCS PosText))) ;�������� ��������� �� ��� � ���
	 		;)
			;����� ������� �� ������ ������
			;������ ��������� ����� ������
			;        		LP2
		  	;                          \
		  	;	       PosText ----->LP1  - ������ ��������� ������� ������
		 	;		      	   /
		  	;			LP3
			(setq DrawDist (* (/ (strlen Grad) 2) H_text 2))
			;���������� ����������� ������� � ������� ������
			(if (= PosLP3 "N")
				(setq DrawAngle DrawAngle)
				(setq DrawAngle (+ DrawAngle PI))
			);if
			;��������� ���������� ������� � ������ �������
			(setq LP1 (polar PosText DrawAngle DrawDist))
			(setq LP2 (polar LP1 (+ DrawAngle (+ PI(/ PI 6))) (* H_text 1.5)))
			(setq LP3 (polar LP1 (- DrawAngle (+ PI(/ PI 6))) (* H_text 1.5)))
				(entmake (list
								(cons 0 "LWPOLYLINE")
								(cons 8 N_layr)
								(cons 62 C_layr)
								(cons 100 "AcDbEntity")
								(cons 100 "AcDbPolyline")
								(cons 90 3)
								(list 10 (car LP2) (cadr LP2))
								(list 10 (car LP1) (cadr LP1))
								(list 10 (car LP3) (cadr LP3))
					));entmake LWPOLYLINE
				`(entmake (list
								(cons 0 "LWPOLYLINE")
								(cons 8 N_layr)
								(cons 62 C_layr)
								(cons 100 "AcDbEntity")
								(cons 100 "AcDbPolyline")
								(cons 90 3)
								(list 10 (car LP1) (cadr LP1))
								(list 10 (car PosText) (cadr PosText))
					));entmake LWPOLYLINE
		);progn
	 );if 
);Print_Grand

;===================================================================================================
; ������� ������ ��������� ����������� ����
;===================================================================================================
(defun Dialog()
  (setq name_dcl "UKL.dcl")
	(setq dcl_id (load_dialog name_dcl))
	(if (= dcl_id -1)
	  (progn
	  (print) (princ "���� ") (princ name_dcl) (princ " �� ������") (print)
	  (exit)
	  )
	);if dcl_id
  	(setq new_dial (new_dialog "dc_ukl" dcl_id))
	  	(if (null new_dial)
			  (progn
					(print "�� ���� ��������� ���������� ����")
					(exit)
			  )
			);End if
	(Initialization)
			(action_tile "ListLayer" "(Input_Layer (get_tile \"ListLayer\"))")
			(action_tile "LayerName" "(Input_Layer_Name (get_tile \"LayerName\"))")
			(action_tile "LayerColor" "(Enter_Color_Take)")
			(action_tile "ListSText" "(Input_SText (get_tile \"ListSText\"))")
			(action_tile "TextHeight" "(Input_HText (get_tile \"TextHeight\"))")
			(action_tile "g1" "(if (= (get_tile \"g1\") \"1\")(setq S_Grad 1))")
			(action_tile "g2" "(if (= (get_tile \"g2\") \"1\")(setq S_Grad 2))")
			(action_tile "g3" "(if (= (get_tile \"g3\") \"1\")(setq S_Grad 3))")
			(action_tile "g4" "(if (= (get_tile \"g4\") \"1\")(setq S_Grad 4))")
			(action_tile "g5" "(if (= (get_tile \"g5\") \"1\")(setq S_Grad 5))")
			(action_tile "prH" "(if (= (get_tile \"prH\") \"1\")(setq S_prHXY 1))")
 			(action_tile "prX" "(if (= (get_tile \"prX\") \"1\")(setq S_prHXY 2))")
			(action_tile "prY" "(if (= (get_tile \"prY\") \"1\")(setq S_prHXY 3))")
  			(action_tile "prXY" "(if (= (get_tile \"prXY\") \"1\")(setq S_prHXY 4))")
			(action_tile "a1" "(if (= (get_tile \"a1\") \"1\")(setq S_Arrow 1))")
			(action_tile "a2" "(if (= (get_tile \"a2\") \"1\")(setq S_Arrow 2))")
			(action_tile "accept" "(done_dialog 1)")
			(action_tile "cancel" "(done_dialog 2)")
	
	(if (= (start_dialog) 1 ) (Enter_Point))
	(if (= (start_dialog) 2 ) (Exit))
	(unload_dialog dcl_id)
); End Dialog

;===================================================================================================
;������� ����� � �������� ��������� ��������
;===================================================================================================

(defun Initialization()
	(ListSTible "LAYER")						;������������ ������ ����
	(setq L_list (cons "�������" L_list))				;� ������ ������ ��������� "�������"
	(ListSTible "STYLE")						;������������ ������ ������ ������
	(start_list "ListLayer")(mapcar 'add_list L_list)(end_list) 	;�������� ������ ���� � ������
	(start_list "ListSText")(mapcar 'add_list S_list)(end_list) 	;�������� ������ ������� ������ � ������
	(set_tile "LayerName" N_layr)					;������� � ������� ��� ����
	(set_tile "TextHeidht" (rtos H_text))				;������� � ������� ������ ������
	
);End Initialization

;;;===================================================================================================
;;; ����� ������ �� ������������ ������ �� �������
;;; List_Tible - ������ ������� string
;;; sType - ����� ��������� ������ "APPID", "BLOCK", "DIMSTYLE", "LAYER", "LTYPE", "STYLE", "UCS",
;;; "VIEW", "VPORT"
;;;===================================================================================================

(defun ListSTible(sType / i temp lay_list lay1 lay2 tmp_elem List_Tible)
  (setq temp T)
  (setq lay_list '())
  (setq lay1 (tblnext sType T))
  (setq lay_list (append lay_list (list lay1)))
  (while (/= temp nil)
		(setq lay2 '())
		(setq lay2 (tblnext sType))
		(setq temp lay2)
		(setq lay_list (append lay_list (list temp)))
  )
  (setq i 0)
  (setq List_Tible '())
  (repeat (length lay_list)
		(setq tmp_elem (cdr (assoc 2 (nth i lay_list))))
		(if (/= tmp_elem nil )(setq List_Tible (append List_Tible (list tmp_elem))))
		(setq i (1+ i))
  );End repeat
	;��������� �������
	(cond
		((= sType "LAYER") (setq L_list List_Tible)) ;���� ����
	  ((= sType "STYLE") (setq S_list List_Tible)) ;���� ����� ������
	);cond 
);End List_layer


;===================================================================================================
;������� �������� ���� ����� ����
;===================================================================================================

(defun Input_Layer_Name(L_Name)
	(if (or (= L_Name "") (null L_Name))
	  (progn
	  	(alert "������� ��� ����")					;���� ������� 
	  );progn
	  (progn
		(setq N_layr L_Name)						;��������� ��� ����
	  );progn
	);if
);End Input_Diam

;===================================================================================================
;������� �������� ������ ���� �� ������
;===================================================================================================

(defun Input_Layer(Layer / i)
	(setq i (atoi Layer))
	(if (> i 0)
		(progn(mode_tile "LayerName" 1)(mode_tile "LayerColor" 1))
		(progn(mode_tile "LayerName" 0)(mode_tile "LayerColor" 0))
	);if
	(setq N_layr (nth i L_list))
	(if (= (atoi (get_tile "ListLayer")) 0) (New_Layer N_layr C_layr))	;�������� ���� ���� ������		
);End Input_Layer

;===================================================================================================
; ������� ������ ����������� ���� ��� ���������� �����
; � ������� ������ ������� ������
; - C_layr
;===================================================================================================
  
(defun Enter_Color_Take(/ dx dy tk_col)
  	(setq dx (dimx_tile "LayerColor") dy (dimy_tile "LayerColor"))
  	(setq tk_col (acad_colordlg C_layr))
  	(if (null tk_col) (princ)
  	(setq C_layr tk_col))
  	(start_image "LayerColor")
  	(fill_image 0 0 dx dy C_layr)
  	(end_image)
);End Enter_Color_Take

;===================================================================================================
;������� �������� ������ ����� ������ �� ������
;===================================================================================================

(defun Input_SText(sText / i)
	(setq i (atoi sText))
	(setq S_text (nth i S_list))
);End Input_Layer

;===================================================================================================
;������� �������� ���� ������ ������
;===================================================================================================

(defun Input_HText(hText)
	(if (or (= hText "") (null hText) (= hText "0"))
	  (progn
	  	(alert "������� ������ ������")		;���� ������� 
	  );progn
	  (progn
		(setq H_text (atof hText))		;��������� ������� ������
	  );progn
	);if
);End Input_Diam

;===================================================================================================
;������� �������� ������ ����
;===================================================================================================

(defun New_Layer(N_layr C_layr)
	(entmake (list
			(cons 0 "LAYER")
			(cons 100 "AcDbSymbolTableRecord")
			(cons 100 "AcDbLayerTableRecord")
			(cons 2 N_layr)
			(cons 70 0)
			(cons 62 C_layr)
			(cons 6 "Continuous")
		);list
	);entmake
);End New_Layer

;===================================================================================================
;������� ������� ������
;===================================================================================================

(defun Calc_Grad(gType /  tmp)
	(cond
		((= gType 1) (setq Grad (rtos (abs (/ Elev Dist))2 4)))				;�� �� ����
		((= gType 2) (setq Grad (rtos (abs (/ Elev Dist))2 2)))				;�� �� ����
		((= gType 3) (setq Grad (strcat (rtos (abs (* (/ Elev Dist) 100))2 3)"%")))	;�������� %
		((= gType 4) (setq Grad (strcat (rtos (abs (* (/ Elev Dist) 1000))2 4)"�")))	;������� �
		((= gType 5)
				(setq tmp (fix (abs (/ Dist Elev))))				;�����������
				(setq Grad (strcat "1:" (rtos tmp 2 0)))			; 1:N
		);gType 5
	);end cond
);End Calc_Grad
;===================================================================================================