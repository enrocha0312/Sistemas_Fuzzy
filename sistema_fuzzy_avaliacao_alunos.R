#Programa para avaliação de um aluno via sistema fuzzy

#instala pacote
install.packages("sets", dependencies=T)

#carrega o pacote
library(sets)

#Define Universo
sets_options("universe", seq(1, 100, 1))

#criacao de variaveis
variaveis <- set(
  Nota = fuzzy_partition(varnames = c(MuitoBaixa= 40, Media = 60, Boa = 75, Otima=85), sd = 10),
  Frequencia = fuzzy_partition(varnames = c( Baixa  = 20, Razoavel= 50, Alta=80), radius=20, FUN = fuzzy_cone),
  Pontualidade = fuzzy_partition(varnames = c(MuitosAtrasos = 20, PoucosAtrasos = 55,  Pontual = 80),  sd = 10),
  Participacao = fuzzy_partition(varnames = c(PoucoParticipativo = 35, Participativo=55  , MuitoParticipativo = 70), sd=10),
  Classificacao = fuzzy_partition(varnames = c(AlunoRuim = 30, AlunoMediano=50, AlunoBom = 65, AlunoOtimo = 80), sd=10)
)

regras <-
  set(
    fuzzy_rule( Nota %is% Otima && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo,  Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Otima && Frequencia %is% Alta && Pontualidade %is% PoucosAtrasos && Participacao %is% MuitoParticipativo,Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Otima && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% Participativo, Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Otima && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% PoucoParticipativo, Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Otima && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo,  Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Otima && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% Participativo, Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Otima && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% MuitoParticipativo,  Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoOtimo ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% Participativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% PoucoParticipativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% Participativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% Participativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Boa && Frequencia %is% Baixa && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoBom ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% Participativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% PoucoParticipativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Alta && Pontualidade %is% PoucosAtrasos && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Alta && Pontualidade %is% PoucosAtrasos && Participacao %is% Participativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% Participativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% Participativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Media && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% PoucoParticipativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Alta && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoMediano ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% Pontual && Participacao %is% Participativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% Participativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% PoucosAtrasos && Participacao %is% PoucoParticipativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% MuitosAtrasos && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% MuitosAtrasos && Participacao %is% Participativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Razoavel && Pontualidade %is% MuitosAtrasos && Participacao %is% PoucoParticipativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Baixa && Pontualidade %is% Pontual && Participacao %is% MuitoParticipativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Baixa && Pontualidade %is% PoucosAtrasos && Participacao %is% Participativo, Classificacao %is% AlunoRuim ),
    fuzzy_rule( Nota %is% Ruim && Frequencia %is% Baixa && Pontualidade %is% MuitosAtrasos && Participacao %is% PoucoParticipativo, Classificacao %is% AlunoRuim )
  )

#construindo o sistema
sistema <- fuzzy_system(variaveis, regras)
sistema
plot(sistema)

inferencia <- fuzzy_inference(sistema, list(Nota  = 35 , Frequencia = 55, Pontualidade = 40, Participacao = 30 ))
inferencia
plot(inferencia)

def = gset_defuzzify(inferencia, "centroid")

#analisando o resultado
plot(sistema$variables$Classificacao)
lines(inferencia, col = "green", lwd=4)


#desfazendo o universo
sets_options("universe", NULL)