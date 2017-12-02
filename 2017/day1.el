;; The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of
;; all digits that match the next digit in the list. The list is circular, so the digit after the
;; last digit is the first digit in the list.

(defun aoc-string-to-numbers (input)
  (mapcar (lambda (c) (- c ?0))
          (string-to-list input)))

(defun aoc-day1-1 (input)
  (let* ((in-list (aoc-string-to-numbers input))
         (prev (car (last in-list)))
         (result 0))
    (dolist (i in-list)
      (when (= i prev)
        (incf result i))
      (setf prev i))
    result))

;; 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
(assert (= 3 (aoc-day1-1 "1122")))
;; 1111 produces 4 because each digit (all 1) matches the next.
(assert (= 4 (aoc-day1-1 "1111")))
;; 1234 produces 0 because no digit matches the next.
(assert (= 0 (aoc-day1-1 "1234")))
;; 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
(assert (= 9 (aoc-day1-1 "91212129")))

;;--------------------------------------------------------------------------------

;; Now, instead of considering the next digit, it wants you to consider the digit halfway around the
;; circular list. That is, if your list contains 10 items, only include a digit in your sum if the
;; digit 10/2 = 5 steps forward matches it. Fortunately, your list has an even number of elements.

(defun aoc-opposite-number (index lst)
  (let ((len (length lst)))
    (nth (mod (+ index (/ len 2)) len) lst)))

(defun aoc-day1-2 (input)
  (let ((in-list (aoc-string-to-numbers input))
        (result 0))
    (dotimes (i (length in-list))
      (let ((cur (nth i in-list)))
        (when (= cur (aoc-opposite-number i in-list))
          (setf result (+ result cur)))))
    result))

;; 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
(assert (= 6 (aoc-day1-2 "1212")))
;; 1221 produces 0, because every comparison is between a 1 and a 2.
(assert (= 0 (aoc-day1-2 "1221")))
;; 123425 produces 4, because both 2s match each other, but no other digit has a match.
(assert (= 4 (aoc-day1-2 "123425")))
;; 123123 produces 12.
(assert (= 12 (aoc-day1-2 "123123")))
;; 12131415 produces 4.
(assert (= 4 (aoc-day1-2 "12131415")))

(setf day1input (concat "567298753335395619962968394156452864626256711743346154774779392832295864677983248468917"
                        "415191826155168922175616559889842873678219451162782935571849372396132327213645251798747"
                        "135138188194688352824861161125865619981299863268266874968358851536294699441585233719671"
                        "847621916212497883653734892459195718882792975341788494213384466463696974254771722825573"
                        "995931635185273159829252983788599278181513187618357846113579131528713524354165985373434"
                        "337661841995277616554482971767698889768414132813834838288269967295786614652475987923655"
                        "593572365532674371354293169347782428928354246863952227164325721283324816539195768622631"
                        "124651797831925397727666382547914432115571286694625599263487615882285538233145264995328"
                        "378886324819233824594396626919742147455577913516863726327957984288534715228727567981157"
                        "659437653522616789498122686622298752241578524487588255641495672497634162712355721483787"
                        "387272361839552973534927324168654828754976399365337953944543531969882546528981766329443"
                        "645819486727862397874598179928378923755524272829133753849861692981726821169864923664612"
                        "789998283952378483775286345881996548514981295912188477184995472325936577815178871994188"
                        "812861855245587936991951131973552562119818563434253884846246183333291798629744538851571"
                        "746316851512373245557614344745483584956575777332536746976338375767793874831996897131226"
                        "787161995165726791381724248555977158216729579425944125628416835629278556885852718412223"
                        "126246519361212796168551391383527482389259692378661329974734725925482353126218532827436"
                        "752926586885651218513532965263593837326675996411986349479822224553675879238978981864665"
                        "528785617353447955136411597681145967712359274737529631366725341369882365521825416819616"
                        "288343738971816774387121637316486542645879423949622485897169487715959121577293839682743"
                        "528973416585397526752129157443656719347381424798187773522337696412535999255588513781664"
                        "738213959664685641742461784798185553291487225168671939434176432439525455678227742632633"
                        "144198173755726258176241254484968947228164583595766721738433443539157298522828653757438"
                        "8834835693416821419655967456137395465649249256572866516984318344482684936625486311718525523265165"))
