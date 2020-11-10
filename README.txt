total=banco de dados por ocorrencia
tentativa$Total= mortalidade dados do dataus
tentativa$n=mortalidade dados da prf agrupado por pessoa e veiculo
mortalidade1=mortalidade com as taxas e com apenas os anos de 2018 e 2019
elemento= imagem da porcentagem dos envolvidos
imagem2= propaganda maio amarelo

fase_dia= df1=total%>%group_by(ano,fase_dia,condicao_metereologica)%>%
  summarise(ileso=sum(ilesos),mortos=sum(mortos),feridos_leves=sum(feridos_leves),
            feridos_graves=sum(feridos_graves))
df1=melt(df1, id.vars = c("condicao_metereologica","fase_dia","ano"), variable.name = "feridos",
         value.name="n")
