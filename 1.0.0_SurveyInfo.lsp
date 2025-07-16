;FAZ PALITO DE SONDAGEM

(defun c:SONDM()
  	(setq prof (getreal "\n digite a profundidade da sondagem:"))
  	;(setq hori (getreal "\n digite a escala horizontal do desenho:"))
  	;(setq vert (getreal "\n digite a escala vertical do desenho:"))
  	(setq nome (getstring "\n digite o nome da sondagem:"))
    	(setq cota (getstring "\n digite a cota da sondagem:"))
  	(setq proj (getstring "\n digite a projeção da sondagem:"))
  	(setq na2 (getstring "\n digite a diferença de cota do nível d'água:"))
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
	(command "text" "j" "MC" p11 1.5 0 nome)                             ;nome da sondagem
  	(command "text" "j" "MC" p12 1.0 0 cota)                             ;cota da sondagem
	(command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " proj "m"))      ;projeção da sondagem

  ;fazer o palito
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
	(setq p19 (polar p14 (* (/ 180.0 180.0) pi) 1.5))        ;Cálculo do ponto direito alto do N.A.
	(setq p20 (polar p19 (* (/ 270.0 180.0) pi) (* 2.5 (atof na2))))  ;Cálculo do ponto direito do N.A.
	(setq p21 (polar p20 (* (/ 180.0 180.0) pi) 1.25))       ;Cálculo do ponto do meio do N.A.
	(setq p22 (polar p21 (* (/ 180.0 180.0) pi) 1.25))       ;Cálculo do ponto do esquerdo do N.A.
	(setq p23 (polar p21 (* (/ 270.0 180.0) pi) 1.25))       ;Cálculo do ponto abaixo do N.A.
  	(command "text" "j" "BC" p21 1 0 "N.A.")        ;escreve o texto
  	(command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
     	(command "_pline" p20 p22 p23 "close")                       ;desenha o contorno do NA
  	(command "_solid" p20 p22 p23 p20 "")                        ;desenha o sólido

;colocar sólidos e textos no palito

  
	(setq p24 (polar p14 (* (/ 270.0 180.0) pi) 2.5))    ;cálculo do ponto baixo à esquerda do 1º sólido
  	(setq p25 (polar p24 (* (/ 0.0 180.0) pi) 0.75))     ;cálculo do ponto baixo à direita do 1º sólido
  	(setq p26 (polar p24 (* (/ 270.0 180.0) pi) 2.5))    ;cálculo do ponto alto à esquerda do sólido seguinte

  	(setq dist1 (distance p14 p16))                              ;distância total do furo
        (setq dist2 (distance p14 p24))                              ;distância do 1º metro
        (setq dist3 (distance p14 p26))                              ;distância alta do sólido seguinte

  
(if (<= dist1 dist2)
  (progn
   	(command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
    	(command "_solid" p14 p15 p16 p17 "")
      	(setq p27 (polar p17 (* (/ 0.0 180.0) pi) 0.5))                   ;Cálculo do ponto direito do texto
      	(command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
     	(command "text" "j" "l" p27 1 0 "-")                              ;coloca o texto dos golpes
    )
  (while (< dist3 dist1)
     (if (<= (+ dist3 2.5) dist1)
       (progn
    	(setq p28 (polar p14 (* (/ 270.0 180.0) pi) 2.5))      ;cálculo do ponto abaixo à esquerda do 1º sólido
        (setq p29 (polar p28 (* (/ 0.0 180.0) pi) 0.75))      ;cálculo do ponto abaixo à direita do 1º sólido
    	(setq p30 (polar p29 (* (/ 0.0 180.0) pi) 0.5))    ;Cálculo do ponto direito do texto
    	(command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
    	(command "_line" p28 p29 "")
      	;(setq p32 (polar p30 (* (/ 270.0 180.0) pi) (* vert2 0.4)))    ;Cálculo do ponto direito do texto
        (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
     	(command "text" "j" "l" p30 1 0 "-")              ;coloca o texto dos golpes
    	(setq p14b (polar p28 (* (/ 270.0 180.0) pi) 2.5))       
        (if (<= (+ dist3 2.5) dist1)
		(progn
		  (setq p15b (polar p14b (* (/ 0.0 180.0) pi) 0.75))
		  (setq p30a (polar p15b (* (/ 0.0 180.0) pi) 0.5))    ;Cálculo do ponto direito do texto
      		  ;(setq p32a (polar p30a (* (/ 270.0 180.0) pi) (* vert2 0.4)))
		  (command "text" "j" "l" p30a 1 0 "-")
		  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
    	          (command "_solid" p28 p29 p14 p15 "")
		  (command "_line" p14b p15b "")
		  (setq p14 (polar p14b (* (/ 180.0 180.0) pi) 0))
		  (setq p15 (polar p15b (* (/ 180.0 180.0) pi) 0))
		  )
		((setq p14 (polar p14b (* (/ 270.0 180.0) pi) 2.5))
		  )
		)
	)
       (progn
		  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
		  (command "_solid" p14b p15b p16 p17 "")
	 	  (setq p17a (polar p17 (* (/ 0.0 180.0) pi) 0.5))
	 	  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
	 	  (command "text" "j" "l" p17a 1 0 "-")
		  (setq p14 (polar p14b (* (/ 270.0 180.0) pi) 2.5))
	 (setq dist3 (distance p14a p14))
		  ))
    (setq dist3 (distance p14a p14))
    )
  )

(setvar "clayer" ml)
  (setvar "osmode" osn)
  (setvar "textstyle" mt)
  ;(setvar "luprec" dec)
  (princ)
  
  
  )
    


  





