;==============================================================================
; GERADOR DE PALITOS DE SONDAGEM COM LEITURA DE DADOS CSV - VERSÃO OTIMIZADA
;==============================================================================
; Descrição: Cria palitos de sondagem no AutoCAD com dados extraídos de arquivo CSV
; Autor: André Buchmann Müller
; Versão: 1.4.0
;;==============================================================================

(defun c:SONDM (/ nome csv-data cota prof proj na2 p1 osn ml mt
                p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18
                p14a p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p30a
                p14b p15b p17a dist1 dist2 dist3 ang90 ang180 ang270 ang0)
  
  ;; Pré-cálculo de ângulos para melhor performance
  (setq ang0 0.0)
  (setq ang90 (/ pi 2))
  (setq ang180 pi)
  (setq ang270 (* 3 (/ pi 2)))
  
  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: CONVERSÃO DE FORMATO DECIMAL
  ;;----------------------------------------------------------------------------
  (defun convert-decimal-format (str)
    (if (and str (> (strlen str) 0))
      (vl-string-subst "." "," str)
      "0"
    )
  )

  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: LEITURA DE DADOS DO ARQUIVO CSV
  ;;----------------------------------------------------------------------------
  (defun read-csv-data (nome-sondagem / csv-path csv-file line-data fields found-data)
    (if (or (not nome-sondagem) (= nome-sondagem ""))
      (return nil)
    )
    
    (setq csv-path (strcat (getvar "DWGPREFIX") "sondagens.csv"))
    
    (if (not (findfile csv-path))
      (return nil)
    )
    
    (setq csv-file (open csv-path "r"))
    (if (not csv-file)
      (return nil)
    )
    
    ;; Ignora cabeçalho
    (read-line csv-file)
    
    ;; Busca eficiente
    (setq found-data nil)
    (setq nome-sondagem (strcase nome-sondagem))
    (while (and (setq line-data (read-line csv-file)) (not found-data))
      (if line-data
        (progn
          (setq fields (split-string line-data ";"))
          (if (and (>= (length fields) 5) (= (strcase (nth 0 fields)) nome-sondagem))
            (setq found-data (list
              (nth 1 fields)
              (atof (convert-decimal-format (nth 2 fields)))
              (nth 3 fields)
              (convert-decimal-format (nth 4 fields))
            ))
          )
        )
      )
    )
    
    (close csv-file)
    found-data
  )
  
  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: DIVISÃO DE STRING
  ;;----------------------------------------------------------------------------
  (defun split-string (str delimiter / result pos)
    (setq result '())
    (while (setq pos (vl-string-search delimiter str))
      (setq result (cons (substr str 1 pos) result))
      (setq str (substr str (+ pos 2)))
    )
    (reverse (cons str result))
  )

  ;;============================================================================
  ;; FUNÇÃO PRINCIPAL
  ;;============================================================================
  
  ;; Entrada de dados
  (setq nome (getstring "\nDigite o nome da sondagem: "))
  (if (or (not nome) (= nome ""))
    (exit)
  )
  
  ;; Leitura CSV
  (setq csv-data (read-csv-data nome))
  (if (not csv-data)
    (progn
      (princ (strcat "\nSondagem " nome " não encontrada."))
      (exit)
    )
  )
  
  ;; Processamento de dados
  (setq cota (nth 0 csv-data))
  (setq prof (nth 1 csv-data))
  (setq proj (nth 2 csv-data))
  (setq na2 (nth 3 csv-data))
  
  (if (or (not (numberp prof)) (<= prof 0))
    (exit)
  )
  
  ;; Seleção do ponto
  (setq p1 (getpoint "\nClique o ponto de inserção: "))
  (if (not p1)
    (exit)
  )
  
  ;; Configuração do ambiente
  (setq osn (getvar "osmode"))
  (setq ml (getvar "clayer"))
  (setq mt (getvar "textstyle"))
  (setvar "osmode" 0)

  ;;============================================================================
  ;; CRIAÇÃO DA BANDEIRA
  ;;============================================================================
  
  ;; Cálculo otimizado dos pontos
  (setq p2 (polar p1 ang90 6.5))
  (setq p3 (polar p2 ang180 6.75))
  (setq p4 (polar p3 ang90 5.0))
  (setq p5 (polar p4 ang0 13.5))
  (setq p6 (polar p5 ang270 5.0))
  (setq p7 (polar p4 ang270 2.0))
  (setq p8 (polar p7 ang0 13.5))
  (setq p9 (polar p7 ang270 1.5))
  (setq p10 (polar p9 ang0 13.5))
  (setq p11 (polar p2 ang90 4.0))
  (setq p12 (polar p2 ang90 2.25))
  (setq p13 (polar p2 ang90 0.75))

  ;; Criação de layers (simplificada)
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  (command "_line" p1 p2 "")
  (command "_pline" p3 p4 p5 p6 "close")
  (command "_line" p7 p8 "")
  (command "_line" p9 p10 "")

  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  (command "text" "j" "MC" p11 1.5 0 nome)
  (command "text" "j" "MC" p12 1.0 0 cota)
  (command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " proj "m"))

  ;;============================================================================
  ;; CRIAÇÃO DO PALITO
  ;;============================================================================
  
  (setq p14 (polar p1 ang180 0.375))
  (setq p15 (polar p1 ang0 0.375))
  (setq p16 (polar p14 ang270 (* prof 2.5)))
  (setq p17 (polar p15 ang270 (* prof 2.5)))
  (setq p18 (polar p1 ang270 (+ (* prof 2.5) 1.7)))
  
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  (command "_pline" p14 p16 p17 p15 "close")
  
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  (command "text" "j" "MC" p18 1.25 0 (rtos prof))
  
  (setq p14a p14)
 
  ;;============================================================================
  ;; CRIAÇÃO DO INDICADOR N.A.
  ;;============================================================================
  
  (setq p19 (polar p14 ang180 1.5))
  (setq p20 (polar p19 ang270 (* 2.5 (atof na2))))
  (setq p21 (polar p20 ang180 1.25))
  (setq p22 (polar p21 ang180 1.25))
  (setq p23 (polar p21 ang270 1.25))
  
  (command "text" "j" "BC" p21 1 0 "N.A.")
  (command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
  (command "_pline" p20 p22 p23 "close")
  (command "_solid" p20 p22 p23 p20 "")

  ;;============================================================================
  ;; CRIAÇÃO DE MARCAÇÕES
  ;;============================================================================
  
  (setq p24 (polar p14 ang270 2.5))
  (setq p26 (polar p24 ang270 2.5))
  (setq dist1 (distance p14 p16))
  (setq dist2 (distance p14 p24))
  (setq dist3 (distance p14 p26))

  (if (<= dist1 dist2)
    ;; Profundidade menor que um metro
    (progn
      (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
      (command "_solid" p14 p15 p16 p17 "")
      (setq p27 (polar p17 ang0 0.5))
      (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
      (command "text" "j" "l" p27 1 0 "-")
    )
    ;; Loop otimizado para profundidade maior
    (while (< dist3 dist1)
      (setq p28 (polar p14 ang270 2.5))
      (setq p29 (polar p28 ang0 0.75))
      (setq p30 (polar p29 ang0 0.5))
      
      (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
      (command "_line" p28 p29 "")
      (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
      (command "text" "j" "l" p30 1 0 "-")
      
      (setq p14b (polar p28 ang270 2.5))
      
      (if (<= (+ dist3 2.5) dist1)
        (progn
          (setq p15b (polar p14b ang0 0.75))
          (setq p30a (polar p15b ang0 0.5))
          (command "text" "j" "l" p30a 1 0 "-")
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_solid" p28 p29 p14 p15 "")
          (command "_line" p14b p15b "")
          (setq p14 p14b)
          (setq p15 p15b)
        )
        (progn
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_solid" p14b p15b p16 p17 "")
          (setq p17a (polar p17 ang0 0.5))
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p17a 1 0 "-")
          (setq p14 (polar p14b ang270 2.5))
        )
      )
      (setq dist3 (distance p14a p14))
    )
  )

  ;;============================================================================
  ;; FINALIZAÇÃO
  ;;============================================================================
  ;; Restaura variáveis do sistema
  (setvar "clayer" ml)
  (setvar "osmode" osn)
  (setvar "textstyle" mt)
  
  (princ "\nPalito criado com sucesso!")
  (princ)
)