;FAZ PALITO DE SONDAGEM




(defun parse-csv (linha / pos campos temp ch)
  (setq pos 0
        campos '()
        temp ""
  )
  (while (< pos (strlen linha))
    (setq ch (substr linha (1+ pos) 1))
    (if (equal ch ";")
      (progn
        (setq campos (append campos (list (limpar-espacos temp))))
        (setq temp "")
      )
      (setq temp (strcat temp ch))
    )
    (setq pos (1+ pos))
  )
  ; adiciona o último campo
  (setq campos (append campos (list (limpar-espacos temp))))
  campos
)

;; Função auxiliar para remover espaços antes e depois de um campo
(defun limpar-espacos (s / start end)
  (setq start 1)
  (while (and (<= start (strlen s)) (wcmatch (substr s start 1) " "))
    (setq start (1+ start))
  )
  (setq end (strlen s))
  (while (and (>= end start) (wcmatch (substr s end 1) " "))
    (setq end (1- end))
  )
  (if (>= end start)
    (substr s start (- end start -1))
    ""
  )
)




(defun c:SONDM()
  	;(setq prof (getreal "\n digite a profundidade da sondagem:"))
  	;(setq hori (getreal "\n digite a escala horizontal do desenho:"))
  	;(setq vert (getreal "\n digite a escala vertical do desenho:"))
  	;(setq nome (getstring "\n digite o nome da sondagem:"))
    	;(setq cota (getstring "\n digite a cota da sondagem:"))
  	;(setq proj (getstring "\n digite a projeção da sondagem:"))
  	;(setq na2 (getstring "\n digite a diferença de cota do nível d'água:"))
  	(setq p1 (getpoint "\n clique o ponto de inserção do palito:"))
	(setq osn (getvar "osmode"))
	(setq ml (getvar "clayer"))
	(setq mt (getvar "textstyle"))
  	;(setq dec (getvar "luprec"))
	(setvar "osmode" 0)
  ;FAZER A BANDEIRA
        (setq p2 (polar p1 (* (/ 90.0 180.0) pi) 6.5))    ;Cálculo da ligação do palito com a bandeira
  	(setq p3 (polar p2 (* (/ 180.0 180.0) pi) 6.75))  ;Cálculo do ponto esquerdo baixo da bandeira
  	(setq p4 (polar p3 (* (/ 90.0 180.0) pi) 5.0))    ;Cálculo do ponto esquerdo alto da bandeira
  	(setq p5 (polar p4 (* (/ 0.0 180.0) pi) 13.5))    ;Cálculo do ponto direito alto da bandeira
  	(setq p6 (polar p5 (* (/ 270.0 180.0) pi) 5.0))   ;Cálculo do ponto direito baixo da bandeira
  	(setq p7 (polar p4 (* (/ 270.0 180.0) pi) 2.0))   ;Cálculo do ponto esquerdo da 1ª divisão da bandeira
    	(setq p8 (polar p7 (* (/ 0.0 180.0) pi) 13.5))    ;Cálculo do ponto direito da 1ª divisão da bandeira
  	(setq p9 (polar p7 (* (/ 270.0 180.0) pi) 1.5))   ;Cálculo do ponto esquerdo da 2ª divisão da bandeira
    	(setq p10 (polar p9 (* (/ 0.0 180.0) pi) 13.5))   ;Cálculo do ponto direito da 2ª divisão da bandeira
        (setq p11 (polar p2 (* (/ 90.0 180.0) pi) 4.0))   ;Cálculo do texto de identificação da sondagem
        (setq p12 (polar p2 (* (/ 90.0 180.0) pi) 2.25))  ;Cálculo do texto da cota da sondagem
        (setq p13 (polar p2 (* (/ 90.0 180.0) pi) 0.75))  ;Cálculo do texto da projeção da sondagem

  	(command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")    ;cria o layer do contorno da bandeira
	(command "_line" p1 p2 "")                                           ;desenha a ligação da bandeira com o palito
  	(command "_pline" p3 p4 p5 p6 "close")                               ;desenha o contorno da bandeira
    	(command "_line" p7 p8 "")                                           ;desenha a 1ª divisão da bandeira
  	(command "_line" p9 p10 "")                                          ;desenha a 2ª divisão da bandeira

    	(command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")    ;cria o layer dos textos
  	(setq linha-desejada 1)
  	(setq coluna-desejada 1)
  (setq arquivo (open "C:\\sondagem\\sondagem.csv" "r")) ; ALTERE o caminho do arquivo aqui
  (setq contador 1) ; começamos da 1ª linha
  (while (and (< contador linha-desejada) (setq linha (read-line arquivo)))
    (setq contador (1+ contador)))
      (setq campos (parse-csv linha))
      (if (>= (length campos) coluna-desejada)
          (setq valor (nth (- coluna-desejada 1) campos)) ; -1 porque indexa em 0
        (princ "\nColuna não encontrada."))
  	
  	(command "text" "j" "MC" p11 1.5 0 valor)                             ;nome da sondagem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq linha-desejada 1)
  	(setq coluna-desejada 2)
  (setq arquivo (open "C:\\sondagem\\sondagem.csv" "r")) ; ALTERE o caminho do arquivo aqui
  (setq contador 1) ; começamos da 1ª linha
  (while (and (< contador linha-desejada) (setq linha (read-line arquivo)))
    (setq contador (1+ contador)))
      (setq campos (parse-csv linha))
      (if (>= (length campos) coluna-desejada)
          (setq valor (nth (- coluna-desejada 1) campos)) ; -1 porque indexa em 0
        (princ "\nColuna não encontrada."))
	
  	(command "text" "j" "MC" p12 1.0 0 valor)                             ;cota da sondagem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(setq linha-desejada 1)
  	(setq coluna-desejada 4)
  (setq arquivo (open "C:\\sondagem\\sondagem.csv" "r")) ; ALTERE o caminho do arquivo aqui
  (setq contador 1) ; começamos da 1ª linha
  (while (and (< contador linha-desejada) (setq linha (read-line arquivo)))
    (setq contador (1+ contador)))
      (setq campos (parse-csv linha))
      (if (>= (length campos) coluna-desejada)
          (setq valor (nth (- coluna-desejada 1) campos)) ; -1 porque indexa em 0
        (princ "\nColuna não encontrada."))
	
	(command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " valor "m"))      ;projeção da sondagem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;fazer o palito
        (setq linha-desejada 1)
  	(setq coluna-desejada 3)
  (setq arquivo (open "C:\\sondagem\\sondagem.csv" "r")) ; ALTERE o caminho do arquivo aqui
  (setq contador 1) ; começamos da 1ª linha
  (while (and (< contador linha-desejada) (setq linha (read-line arquivo)))
    (setq contador (1+ contador)))
      (setq campos (parse-csv linha))
      (if (>= (length campos) coluna-desejada)
          (setq valor (nth (- coluna-desejada 1) campos)) ; -1 porque indexa em 0
        (princ "\nColuna não encontrada."))
	 
    (setq prof (atof (vl-string-subst "." "," valor)))
 
  	(setq p14 (polar p1 (* (/ 180.0 180.0) pi) 0.375))             ;Cálculo do ponto esquerdo alto do palito
	(setq p15 (polar p1 (* (/ 0.0 180.0) pi) 0.375))               ;Cálculo do ponto direito alto do palito
 	(setq p16 (polar p14 (* (/ 270.0 180.0) pi) (* prof 2.5)))     ;Cálculo do ponto esquerdo baixo do palito
	(setq p17 (polar p15 (* (/ 270.0 180.0) pi) (* prof 2.5)))     ;Cálculo do ponto direito baixo do palito
  	(setq p18 (polar p1 (* (/ 270.0 180.0) pi) (+ (* prof 2.5) 1.7)))   ;Cálculo do ponto do texto da profundidade do palito
  	(command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
   	(command "_pline" p14 p16 p17 p15 "close")                          ;desenha o contorno do palito
  	(command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
	(command "text" "j" "MC" p18 1.25 0 (rtos prof))                    ;coloca o comprimento da sondagem no fundo
        (setq p14a (polar p14 (* (/ 180.0 180.0) pi) (* hori 0)))
;fazer N.A.
	(setq linha-desejada 1)
  	(setq coluna-desejada 5)
  (setq valor (ler-csv-celula 1 5))
	
 
    (setq na2 (atof (vl-string-subst "." "," valor)))

	(setq p19 (polar p14 (* (/ 180.0 180.0) pi) 1.5))        ;Cálculo do ponto direito alto do N.A.
	(setq p20 (polar p19 (* (/ 270.0 180.0) pi) (* 2.5 na2)))  ;Cálculo do ponto direito do N.A.
	(setq p21 (polar p20 (* (/ 180.0 180.0) pi) 1.25))       ;Cálculo do ponto do meio do N.A.
	(setq p22 (polar p21 (* (/ 180.0 180.0) pi) 1.25))       ;Cálculo do ponto do esquerdo do N.A.
	(setq p23 (polar p21 (* (/ 270.0 180.0) pi) 1.25))       ;Cálculo do ponto abaixo do N.A.
  	(command "text" "j" "BC" p21 1 0 "N.A.")        ;escreve o texto
  	(command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
     	(command "_pline" p20 p22 p23 "close")                       ;desenha o contorno do NA
  	(command "_solid" p20 p22 p23 p20 "")                        ;desenha o sólido

  
  
(setvar "clayer" ml)
  (setvar "osmode" osn)
  (setvar "textstyle" mt)
  ;(setvar "luprec" dec)
  (princ)
  
  
  )
    

;(defun c:ler-csv-celula (linha-desejada coluna-desejada / arquivo linha contador campos)
  ;; Configure aqui a linha e coluna desejada (1-baseado)
  ;(setq linha-desejada 2) ; Ex: linha 3
  ;(setq coluna-desejada 3) ; Ex: coluna B (2ª)

  ;; Abre o arquivo CSV
  ;(setq arquivo (open "C:\\sondagem\\sondagem.csv" "r")) ; ALTERE o caminho do arquivo aqui

  ;; Lê até a linha desejada
  ;(setq contador 1) ; começamos da 1ª linha
  ;(while (and (< contador linha-desejada) (setq linha (read-line arquivo)))
    ;(setq contador (1+ contador))
  ;)

  ;(if (= contador linha-desejada)
    ;(progn
      ;; Se encontrou a linha, processa as colunas
      ;(setq campos (parse-csv linha))
      ;(if (>= (length campos) coluna-desejada)
        ;(progn
          ;(setq valor (nth (- coluna-desejada 1) campos)) ; -1 porque indexa em 0
          ;(princ valor)
        ;)
        ;(princ "\nColuna não encontrada.")
     ; )
    ;)
    ;(princ "\nLinha não encontrada.")
  ;)
;)

;(defun c:ler-csv-celula (linha-desejada coluna-desejada / arquivo linha contador campos valor)
  ;(setq arquivo (open "C:\\sondagem\\sondagem.csv" "r")) ; ALTERE o caminho do arquivo aqui
  ;(setq contador 1) ; começamos da 1ª linha
  ;(while (and (< contador linha-desejada) (setq linha (read-line arquivo)))
    ;(setq contador (1+ contador)))
    ;  (setq campos (parse-csv linha))
      ;(if (>= (length campos) coluna-desejada)
         ; (setq valor (nth (- coluna-desejada 1) campos)) ; -1 porque indexa em 0
        ;(princ "\nColuna não encontrada.")))