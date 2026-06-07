;;; ============================================================
;;;  NUM.lsp  —  תוסף מספור אובייקטים
;;;  פקודות: NUM , NUMSET , NUMCHECK
;;;  קובץ זה מכיל את כל קוד התוסף.
;;; ============================================================

(vl-load-com)

;;; ----- שמות קבועים -----
(setq *NUM-DICT* "NUM_SETTINGS")
(setq *NUM-XAPP* "NUM_TAG")
(setq *NUM-VER*  "1")

;;; ============================================================
;;;  רישום אפליקציית
;;;  XData
;;; ============================================================
(defun num:regapp ()
  (if (not (tblsearch "APPID" *NUM-XAPP*))
    (regapp *NUM-XAPP*))
)

;;; ============================================================
;;;  הגדרות — קריאה / כתיבה במילון הקובץ
;;;  סדר השדות:
;;;    0 שכבה   1 גובה   2 סגנון-טקסט
;;;    3 מרחק   4 קידומת  5 סיומת
;;;    6 מספר-התחלה       7 מספר-אחרון
;;; ============================================================
(defun num:get-settings ( / dicts d xrec data res )
  (setq dicts (namedobjdict)
        d     (dictsearch dicts *NUM-DICT*))
  (if d
    (progn
      (setq xrec (cdr (assoc -1 d))
            data (entget xrec)
            res  '())
      (foreach pair data
        (if (= 1 (car pair))
          (setq res (cons (cdr pair) res))))
      (reverse res))
    nil)
)

(defun num:put-settings ( lst / dicts xrec data )
  (setq dicts (namedobjdict))
  (if (dictsearch dicts *NUM-DICT*)
    (dictremove dicts *NUM-DICT*))
  (setq data (list '(0 . "XRECORD") '(100 . "AcDbXrecord")))
  (foreach s lst
    (setq data (append data (list (cons 1 s)))))
  (setq xrec (entmakex data))
  (dictadd dicts *NUM-DICT* xrec)
  lst
)

(defun num:default-settings ()
  (list "NUM_TEXT" "2.5" "Standard" "1.0" "" "" "1" "0")
)

;;; ============================================================
;;;  רשימות שכבות וסגנונות בקובץ
;;; ============================================================
(defun num:layer-list ( / l name )
  (setq l '())
  (setq name (tblnext "LAYER" t))
  (while name
    (setq l (cons (cdr (assoc 2 name)) l))
    (setq name (tblnext "LAYER" nil)))
  (acad_strlsort l)
)

(defun num:style-list ( / l name )
  (setq l '())
  (setq name (tblnext "STYLE" t))
  (while name
    (setq l (cons (cdr (assoc 2 name)) l))
    (setq name (tblnext "STYLE" nil)))
  (if l (acad_strlsort l) (list "Standard"))
)

;;; אינדקס פריט ברשימה
(defun num:idx ( item lst / i found )
  (setq i 0 found 0)
  (foreach x lst
    (if (= x item) (setq found i))
    (setq i (1+ i)))
  found
)

;;; ============================================================
;;;  ודא הגדרות — בפעם הראשונה פותח דיאלוג אוטומטי
;;; ============================================================
(defun num:ensure-settings ( / s )
  (setq s (num:get-settings))
  (if (not s)
    (progn
      (princ "\nפעם ראשונה בקובץ — הגדר פרמטרי מספור.")
      (setq s (num:dlg (num:default-settings)))
      (if s
        (num:put-settings s)
        (setq s (num:put-settings (num:default-settings))))))
  s
)

;;; ============================================================
;;;  XData — קריאה
;;;  טקסט:   גרסה + מספר + handle-מקור
;;;  אובייקט: גרסה + מספר + handle-טקסט
;;; ============================================================
(defun num:read-xdata ( ent / xd app )
  (setq xd (assoc -3 (entget ent (list *NUM-XAPP*))))
  (if xd
    (progn
      (setq app (assoc *NUM-XAPP* (cdr xd)))
      (if app (mapcar 'cdr (cddr app)) nil))
    nil)
)

(defun num:read-text-tag ( ent / f )
  (setq f (num:read-xdata ent))
  (if (and f (>= (length f) 2)) (list (car f) (cadr f)) nil)
)

(defun num:read-orig-tag ( ent / f )
  (setq f (num:read-xdata ent))
  (if (and f (>= (length f) 2)) (list (car f) (cadr f)) nil)
)

;;; ============================================================
;;;  XData — כתיבה (מחיקת כפילויות לפני כתיבה)
;;; ============================================================
(defun num:tag-text ( ent num-str orig-handle / ed )
  (setq ed (entget ent (list *NUM-XAPP*)))
  (setq ed (vl-remove-if '(lambda (x) (= (car x) -3)) ed))
  (entmod (append ed
    (list (list -3 (list *NUM-XAPP*
      (cons 1000 *NUM-VER*)
      (cons 1000 num-str)
      (cons 1000 orig-handle))))))
  (entupd ent)
)

(defun num:tag-orig ( ent num-str txt-handle / ed )
  (setq ed (entget ent (list *NUM-XAPP*)))
  (setq ed (vl-remove-if '(lambda (x) (= (car x) -3)) ed))
  (entmod (append ed
    (list (list -3 (list *NUM-XAPP*
      (cons 1000 *NUM-VER*)
      (cons 1000 num-str)
      (cons 1000 txt-handle))))))
  (entupd ent)
)

;;; ============================================================
;;;  ניקוי כל תגיות NUM_TAG + איפוס מספר אחרון
;;; ============================================================
(defun num:clear-all ( s / ss i ent )
  (setq ss (ssget "X" (list (list -3 (list *NUM-XAPP*)))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (vl-catch-all-apply
          '(lambda ()
             (entmod (append (entget ent)
                             (list (list -3 (list *NUM-XAPP*)))))
             (entupd ent)))
        (setq i (1+ i)))))
  (num:put-settings
    (list (nth 0 s) (nth 1 s) (nth 2 s) (nth 3 s)
          (nth 4 s) (nth 5 s) (nth 6 s) "0"))
)

;;; ============================================================
;;;  גיאומטריה
;;; ============================================================
(defun num:bbox ( ent / obj minArr maxArr res )
  (setq res
    (vl-catch-all-apply
      '(lambda ()
         (setq obj (vlax-ename->vla-object ent))
         (vla-GetBoundingBox obj 'minArr 'maxArr)
         (list (vlax-safearray->list minArr)
               (vlax-safearray->list maxArr)))))
  (if (vl-catch-all-error-p res) nil res)
)

(defun num:text-pt ( ent dist / bb minPt maxPt ep )
  (setq bb (num:bbox ent))
  (if bb
    (progn
      (setq minPt (car bb) maxPt (cadr bb))
      (list (- (car minPt) dist)
            (cadr maxPt)
            (if (caddr minPt) (caddr minPt) 0.0)))
    (progn
      (setq ep (cdr (assoc 10 (entget ent))))
      (if ep ep '(0.0 0.0 0.0))))
)

(defun num:center ( ent / bb minPt maxPt ep )
  (setq bb (num:bbox ent))
  (if bb
    (progn
      (setq minPt (car bb) maxPt (cadr bb))
      (list (/ (+ (car minPt)  (car maxPt))  2.0)
            (/ (+ (cadr minPt) (cadr maxPt)) 2.0)))
    (progn
      (setq ep (cdr (assoc 10 (entget ent))))
      (if ep (list (car ep) (cadr ep)) '(0.0 0.0))))
)

;;; ============================================================
;;;  מיון: מימין לשמאל, מלמעלה למטה
;;; ============================================================
(defun num:sort-rl-tb ( ents th / row-tol pts )
  (setq row-tol (max 5.0 (* th 2.0)))
  (setq pts (mapcar
    '(lambda (e / c) (setq c (num:center e)) (list (car c) (cadr c) e))
    ents))
  (setq pts (vl-sort pts
    '(lambda (a b)
       (if (> (abs (- (cadr a) (cadr b))) row-tol)
         (> (cadr a) (cadr b))
         (> (car a)  (car b))))))
  (mapcar 'caddr pts)
)

;;; ============================================================
;;;  יצירת ישות טקסט
;;; ============================================================
(defun num:mk-text ( ins txt th lay sty )
  (entmakex
    (list '(0 . "TEXT")
          (cons 8  lay)
          (cons 7  sty)
          (cons 10 ins)
          (cons 11 ins)
          (cons 40 th)
          (cons 1  txt)
          '(72 . 0)
          '(73 . 0)))
)

;;; ============================================================
;;;  עדכון כל טקסטי המספור לפי הגדרות נוכחיות
;;; ============================================================
(defun num:update-all ( s / ss i ent tag lay th sty dist pfx sfx
                          num-str orig-h orig-ent ins ed txt-str )
  (setq lay  (nth 0 s)  th   (atof (nth 1 s))
        sty  (nth 2 s)  dist (atof (nth 3 s))
        pfx  (nth 4 s)  sfx  (nth 5 s))
  (setq ss (ssget "X" (list (list -3 (list *NUM-XAPP*)))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (= "TEXT" (cdr (assoc 0 (entget ent))))
          (progn
            (setq tag (num:read-text-tag ent))
            (if tag
              (progn
                (setq num-str (car tag)
                      orig-h  (cadr tag))
                (setq orig-ent
                  (if (and orig-h (not (= orig-h "")))
                    (vl-catch-all-apply 'handent (list orig-h))
                    nil))
                (if (and orig-ent
                         (not (vl-catch-all-error-p orig-ent))
                         (entget orig-ent))
                  (setq ins (num:text-pt orig-ent dist))
                  (setq ins (cdr (assoc 10 (entget ent)))))
                (setq txt-str (strcat pfx num-str sfx)
                      ed      (entget ent))
                (setq ed (subst (cons 8  lay)     (assoc 8  ed) ed))
                (setq ed (subst (cons 7  sty)     (assoc 7  ed) ed))
                (setq ed (subst (cons 40 th)      (assoc 40 ed) ed))
                (setq ed (subst (cons 1  txt-str) (assoc 1  ed) ed))
                (setq ed (subst (cons 10 ins)     (assoc 10 ed) ed))
                (setq ed (subst (cons 11 ins)     (assoc 11 ed) ed))
                (entmod ed)
                (entupd ent)))))
        (setq i (1+ i)))))
)

;;; ============================================================
;;;  מחיקת כל טקסטי המספור של אובייקט — כולל עותקים
;;;  מחפש לפי ה-handle של האובייקט המקורי
;;; ============================================================
(defun num:delete-texts-for ( ent / orig-h ss i txt tag )
  (setq orig-h (cdr (assoc 5 (entget ent))))
  (setq ss (ssget "X" (list (list -3 (list *NUM-XAPP*)) '(0 . "TEXT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq txt (ssname ss i)
              tag (num:read-text-tag txt))
        (if (and tag (= (cadr tag) orig-h))
          (vl-catch-all-apply 'entdel (list txt)))
        (setq i (1+ i)))))
)

;;; ============================================================
;;;  בדיקה: פוליגון סגור או עיגול
;;; ============================================================
(defun num:is-valid-obj ( ent / ed type )
  (setq ed   (entget ent)
        type (cdr (assoc 0 ed)))
  (or (= type "CIRCLE")
      (and (= type "LWPOLYLINE")
           (= 1 (logand 1 (cdr (assoc 70 ed))))))
)

;;; ============================================================
;;;  מספור אובייקט אחד
;;; ============================================================
(defun num:number-one ( ent num s / lay th sty dist pfx sfx
                         old old-txt-h old-txt-ent old-alive
                         ans ins txt-str txt-ent txt-h orig-h proceed )
  (setq lay  (nth 0 s)  th   (atof (nth 1 s))
        sty  (nth 2 s)  dist (atof (nth 3 s))
        pfx  (nth 4 s)  sfx  (nth 5 s)
        proceed t)
  ; בדוק אם האובייקט כבר ממוספר
  (setq old (num:read-orig-tag ent))
  (if old
    (progn
      (setq old-txt-h (cadr old))
      ; בדוק אם הטקסט הישן קיים בשרטוט
      (setq old-txt-ent
        (if (and old-txt-h (not (= old-txt-h "")))
          (vl-catch-all-apply 'handent (list old-txt-h))
          nil))
      (setq old-alive
        (and old-txt-ent
             (not (vl-catch-all-error-p old-txt-ent))
             (entget old-txt-ent)))
      (if old-alive
        ; הטקסט קיים — שאל את המשתמש
        (progn
          (initget "Yes No")
          (setq ans (getkword
            (strcat "\nהאובייקט כבר ממוספר כ-" pfx (car old) sfx
                    ". למספר מחדש? [Yes/No] <No>: ")))
          (if (not (= ans "Yes"))
            (progn (princ " — מדולג.") (setq proceed nil))))
        ; הטקסט נמחק — ממשיך ללא שאלה
        nil)))
  (if proceed
    (progn
      ; מחק את כל הטקסטים של אובייקט זה (כולל עותקים)
      (num:delete-texts-for ent)
      ; צור טקסט חדש
      (setq txt-str (strcat pfx (itoa num) sfx)
            ins     (num:text-pt ent dist)
            orig-h  (cdr (assoc 5 (entget ent)))
            txt-ent (num:mk-text ins txt-str th lay sty))
      (if (not txt-ent)
        (progn (princ "\n[NUM] שגיאה: לא ניתן ליצור טקסט.") nil)
        (progn
          (setq txt-h (cdr (assoc 5 (entget txt-ent))))
          (num:tag-text txt-ent (itoa num) orig-h)
          (num:tag-orig ent     (itoa num) txt-h)
          t)))
    nil)
)

;;; ============================================================
;;;  מספור — לוגיקה משותפת לאחר בחירת האובייקטים
;;; ============================================================
(defun num:number-list ( ents s / cur-num ent ok count new-s )
  (setq cur-num
    (if (= (nth 7 s) "0")
      (atoi (nth 6 s))
      (1+ (atoi (nth 7 s)))))
  (setq count 0 new-s s)
  (foreach ent ents
    (setq ok (num:number-one ent cur-num new-s))
    (if ok
      (progn
        (setq new-s (num:put-settings
          (list (nth 0 new-s) (nth 1 new-s) (nth 2 new-s) (nth 3 new-s)
                (nth 4 new-s) (nth 5 new-s) (nth 6 new-s) (itoa cur-num))))
        (setq cur-num (1+ cur-num))
        (setq count   (1+ count)))))
  (princ (strcat "\n✓ " (itoa count) " אובייקטים מוספרו."))
)

;;; ============================================================
;;;  פעולת "בחר אובייקטים"
;;; ============================================================
(defun num:do-objects ( s / ss i ents ent )
  (princ "\nבחר אובייקטים למספור: ")
  (setq ss (ssget))
  (if (not ss)
    (princ "\nבוטל.")
    (progn
      (setq ents '() i 0)
      (while (< i (sslength ss))
        (setq ents (append ents (list (ssname ss i))))
        (setq i (1+ i)))
      (setq ents (vl-remove-if-not 'num:is-valid-obj ents))
      (if (not ents)
        (princ "\nלא נבחרו פוליגונים סגורים או עיגולים.")
        (progn
          (setq ents (num:sort-rl-tb ents (atof (nth 1 s))))
          (num:number-list ents s)))))
)

;;; ============================================================
;;;  פעולת "בחירה במרובע"
;;;  משמאל לימין = חלון | מימין לשמאל = חוצה
;;; ============================================================
(defun num:do-window ( s / pt1 pt2 mode ss i ent ents )
  (setq pt1 (getpoint "\nפינה ראשונה: "))
  (if (not pt1)
    (princ "\nבוטל.")
    (progn
      (setq pt2 (getcorner pt1 "\nפינה שנייה: "))
      (if (not pt2)
        (princ "\nבוטל.")
        (progn
          (setq mode (if (< (car pt1) (car pt2)) "W" "C"))
          (setq ss (ssget mode pt1 pt2 '((0 . "LWPOLYLINE,CIRCLE"))))
          (if (not ss)
            (princ "\nלא נמצאו אובייקטים.")
            (progn
              (setq ents '() i 0)
              (while (< i (sslength ss))
                (setq ent (ssname ss i))
                (if (num:is-valid-obj ent)
                  (setq ents (append ents (list ent))))
                (setq i (1+ i)))
              (if (not ents)
                (princ "\nלא נמצאו פוליגונים סגורים או עיגולים.")
                (progn
                  (setq ents (num:sort-rl-tb ents (atof (nth 1 s))))
                  (num:number-list ents s)))))))))
)

;;; ============================================================
;;;  פעולת "סורק מספרים"
;;; ============================================================
(defun num:do-check ( s / ss i ent type tag
                       no-text-list valid-nums
                       min-n max-n seq-gaps
                       ans txt-h txt-ent num-str num-int )
  (princ "\nסורק מספרים בשרטוט...")
  (setq ss (ssget "X" (list (list -3 (list *NUM-XAPP*)))))
  (if (not ss)
    (princ "\nלא נמצאו אובייקטים ממוספרים.")
    (progn
      ; --- סרוק אובייקטים מקוריים ---
      (setq no-text-list '()
            valid-nums   '()
            i 0)
      (while (< i (sslength ss))
        (setq ent  (ssname ss i)
              type (cdr (assoc 0 (entget ent))))
        (if (member type '("LWPOLYLINE" "CIRCLE"))
          (progn
            (setq tag (num:read-orig-tag ent))
            (if tag
              (progn
                (setq num-str (car tag)
                      txt-h   (cadr tag)
                      num-int (atoi num-str))
                (setq txt-ent
                  (if (and txt-h (not (= txt-h "")))
                    (vl-catch-all-apply 'handent (list txt-h))
                    nil))
                (if (or (not txt-ent)
                        (vl-catch-all-error-p txt-ent)
                        (not (entget txt-ent)))
                  (setq no-text-list (cons (list num-int ent) no-text-list))
                  (setq valid-nums   (cons num-int valid-nums)))))))
        (setq i (1+ i)))
      ; --- סיכום ---
      (princ (strcat "\nנמצאו "
                     (itoa (+ (length valid-nums) (length no-text-list)))
                     " אובייקטים ממוספרים"))
      (if valid-nums
        (princ (strcat "  |  טקסטים קיימים: " (itoa (length valid-nums)))))
      (if no-text-list
        (princ (strcat "  |  טקסטים חסרים: " (itoa (length no-text-list)))))
      ; --- שחזר טקסטים שנמחקו ---
      (if no-text-list
        (progn
          (princ (strcat "\n⚠ " (itoa (length no-text-list))
                         " אובייקטים שהמספר שלהם נמחק:"))
          (foreach item no-text-list
            (princ (strcat "\n   מספר " (itoa (car item)))))
          (initget "Yes No")
          (setq ans (getkword "\nלשחזר? [Yes/No] <Yes>: "))
          (if (not (= ans "No"))
            (progn
              (foreach item no-text-list
                (num:number-one (cadr item) (car item) s)
                (setq valid-nums (cons (car item) valid-nums)))
              (princ "\n✓ המספרים שוחזרו.")))))
      ; --- חסרים ברצף ---
      (if valid-nums
        (progn
          (setq min-n    (atoi (nth 6 s))
                max-n    (apply 'max valid-nums)
                seq-gaps '()
                i        min-n)
          (while (<= i max-n)
            (if (not (member i valid-nums))
              (setq seq-gaps (append seq-gaps (list i))))
            (setq i (1+ i)))
          (if seq-gaps
            (progn
              (princ (strcat "\n⚠ חסרים ברצף (" (itoa (length seq-gaps)) "): "))
              (foreach g seq-gaps (princ (strcat (itoa g) "  ")))
              (initget "Yes No")
              (setq ans (getkword "\nלמספר אובייקט עבור כל חסר? [Yes/No] <No>: "))
              (if (= ans "Yes")
                (progn
                  (foreach g seq-gaps
                    (princ (strcat "\nבחר אובייקט למספר " (itoa g) ": "))
                    (setq ent (car (entsel)))
                    (if ent
                      (progn (num:number-one ent g s) (princ " ✓"))
                      (princ "  — מדולג")))
                  (princ "\nמילוי הושלם."))))
            (if (not no-text-list)
              (princ (strcat "\n✓ הכל תקין. רצף: "
                             (itoa min-n) " עד " (itoa max-n)))))))
      ; --- איפוס מלא ---
      (initget "Yes No")
      (setq ans (getkword "\nלמחוק את כל המספרים ולהתחיל מחדש? [Yes/No] <No>: "))
      (if (= ans "Yes")
        (progn
          (num:clear-all s)
          (princ "\n✓ כל המספרים נוקו. הפעל NUM → Objects למספור חדש.")))))
  (princ)
)

;;; ============================================================
;;;  הפקודה הראשית NUM
;;; ============================================================
(defun c:NUM ( / s opt )
  (num:regapp)
  (setq s (num:ensure-settings))
  (initget "Settings Objects Window Check")
  (setq opt (getkword "\n[Settings/Objects/Window/Check] <Objects>: "))
  (if (not opt) (setq opt "Objects"))
  (cond
    ((= opt "Settings")
     (setq s (num:dlg s))
     (if s
       (progn
         (num:put-settings s)
         (num:update-all s)
         (princ "\nההגדרות נשמרו וכל הטקסטים עודכנו."))))
    ((= opt "Objects") (num:do-objects s))
    ((= opt "Window")  (num:do-window  s))
    ((= opt "Check")   (num:do-check   s)))
  (princ)
)

;;; ============================================================
;;;  NUMSET — קיצור להגדרות
;;; ============================================================
(defun c:NUMSET ( / s )
  (num:regapp)
  (setq s (num:get-settings))
  (if (not s) (setq s (num:default-settings)))
  (setq s (num:dlg s))
  (if s
    (progn
      (num:put-settings s)
      (num:update-all s)
      (princ "\nההגדרות נשמרו וכל הטקסטים עודכנו."))
    (princ "\nבוטל."))
  (princ)
)

;;; ============================================================
;;;  NUMCHECK — קיצור לסריקה
;;; ============================================================
(defun c:NUMCHECK ( / s )
  (num:regapp)
  (setq s (num:ensure-settings))
  (num:do-check s)
  (princ)
)

;;; ============================================================
;;;  חלון הגדרות (DCL)
;;; ============================================================
(defun num:write-dcl ( / f path )
  (setq path (vl-filename-mktemp "num" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "num_dlg : dialog {" f)
  (write-line "  label = \"הגדרות NUM\";" f)
  (write-line "  : popup_list { key=\"lay\"; label=\"שכבת טקסט\"; }" f)
  (write-line "  : popup_list { key=\"sty\"; label=\"סגנון טקסט\"; }" f)
  (write-line "  : edit_box { key=\"th\";    label=\"גובה טקסט\";     edit_width=10; }" f)
  (write-line "  : edit_box { key=\"dist\";  label=\"מרחק מאובייקט\"; edit_width=10; }" f)
  (write-line "  : edit_box { key=\"pfx\";   label=\"קידומת\";        edit_width=12; }" f)
  (write-line "  : edit_box { key=\"sfx\";   label=\"סיומת\";         edit_width=12; }" f)
  (write-line "  : edit_box { key=\"start\"; label=\"מספר התחלה\";    edit_width=10; }" f)
  (write-line "  : button { key=\"newlay\"; label=\"שכבה חדשה...\"; alignment=left; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path
)

(defun num:dlg ( cur / dclid path lays stys res result )
  (setq lays (num:layer-list)
        stys (num:style-list))
  (if (not (member (nth 0 cur) lays)) (setq lays (cons (nth 0 cur) lays)))
  (if (not (member (nth 2 cur) stys)) (setq stys (cons (nth 2 cur) stys)))
  (setq path (num:write-dcl)
        res  nil)
  (while
    (progn
      (setq dclid (load_dialog path))
      (if (not (new_dialog "num_dlg" dclid))
        (progn (unload_dialog dclid) nil)
        (progn
          (start_list "lay") (mapcar 'add_list lays) (end_list)
          (start_list "sty") (mapcar 'add_list stys) (end_list)
          (set_tile "lay"   (itoa (num:idx (nth 0 cur) lays)))
          (set_tile "sty"   (itoa (num:idx (nth 2 cur) stys)))
          (set_tile "th"    (nth 1 cur))
          (set_tile "dist"  (nth 3 cur))
          (set_tile "pfx"   (nth 4 cur))
          (set_tile "sfx"   (nth 5 cur))
          (set_tile "start" (nth 6 cur))
          (action_tile "newlay" "(setq res \"NEWLAYER\")(done_dialog 2)")
          (action_tile "accept"
            (strcat
              "(setq res (list"
              " (nth (atoi (get_tile \"lay\")) lays)"
              " (get_tile \"th\")"
              " (nth (atoi (get_tile \"sty\")) stys)"
              " (get_tile \"dist\")"
              " (get_tile \"pfx\")"
              " (get_tile \"sfx\")"
              " (get_tile \"start\")"
              " \"" (nth 7 cur) "\"))"
              "(done_dialog 1)"))
          (action_tile "cancel" "(setq res nil)(done_dialog 0)")
          (setq result (start_dialog))
          (unload_dialog dclid)
          (if (= res "NEWLAYER")
            (progn
              (setq res nil)
              (if (vl-catch-all-error-p
                    (vl-catch-all-apply 'command (list "_.CLASSICLAYER")))
                (command "_.LAYER"))
              (getstring "\n[NUM] סיימת ליצור שכבות? לחץ Enter: ")
              (setq lays (num:layer-list))
              t)
            nil))))
  )
  (vl-file-delete path)
  res
)

(princ "\n=== NUM נטען. פקודות: NUM , NUMSET , NUMCHECK ===")
(princ)
