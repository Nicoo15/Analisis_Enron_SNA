
Este es un md para que pueda analizar por mi cuenta los tweets y asi poder sacar mejores conclusiones
# Practica2


Se trata de poner foco en algunos aspectos concretos del caso Enron a partir del dataset y grafo que hemos generado en las clases. Planteo al menos dos alternativas -aunque estoy abierto a propuestas individuales, siempre y cuando se contemplen las metodologías y herramientas que hemos usado en este tema:



Análisis de una comunidad, a partir de las comunidades que se calculan mediante igraph / gephi, describiendo sus actividades, componentes (a partir de información de cargo y rol), métricas individuales, etc. Todo lo que contribuya a clarificar la actividad dentro de la comunidad.
Análisis de momento(s) concreto(s) del tiempo, a partir de los relatos publicados de la quiebra y todos los acontecimientos previos. En este sentido, si encontráis "cascadas" (un hecho que desencadena una reacción en el grupo / comunidad) sería especialmente interesante.


Y por supuesto, una combinación de los dos.



Entrega: código con el que se ha desarrollado la tarea + visualizaciones + documento con conclusiones.



Como digo en clase, para mi lo esencial es extraer conocimiento de los análisis realizados. Las conclusiones son esenciales.

Usa y sigue el ejemplo dado TrabajoEjemploASeguir.md

```{r}


---
title: "Practica2"
output: montejano_nicolas.html
date: "2024-05-09"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

library(igraph)
library(dplyr)
library(ggraph)
library(tidyverse)
library(readr)

```

```{r}
#Cargamos las metricas 01 previamente generadas

load("C:/Users/nicom/OneDrive/Escritorio/U-tad/3ºINSO/Busqueda y Analisis de la Informacion/Practica2/01_metricas.rda")
```

```{r}

Después de un análisis detallado de los datos disponibles, he decidido focalizarme en un intervalo de tiempo que considero crucial. He decidido dirigir mi atención hacia los correos de Jeff Skilling, un CEO destacado en la historia de Enron. Skilling, quien tuvo un papel relevante en los procedimientos judiciales de la empresa, fue hallado culpable de tráfico de información privilegiada en 2003 y ahora colabora con las autoridades mientras espera su sentencia. En su confesión, Skilling admitió tener conocimiento sobre la manipulación de reservas para ocultar pérdidas en Enron Energy Services. Esto me ha intrigado y por ello he decidido investigar algunos de sus correos electrónicos. Nos enfocaremos en tanto los correos que Skilling recibió como los que envió, partiendo desde la fecha indicada.

```{r}
mails.skilling <- edges.full[(edges.full$sender == "jeff.skilling@enron.com" &

as.Date(edges.full$date.R) > "2001-07-01 00:00:00") |
(edges.full$receiver == "jeff.skilling@enron.com" &
as.Date(edges.full$date.R) > "2001-07-01 00:00:00")
,]

mails.skilling <- mails.skilling [order(as.Date(mails.skilling$date.R)),]
nrow(mails.skilling)
```
##[1] 90

Hay 90 correos electrónicos en total que Jeff Skilling envió o recibió después del 1 de julio de 2001. A continuación, analizaremos los correos electrónicos que Jeff Skilling envió y recibió después de esta fecha.

```{r}
head(mails.skilling)
```

[ 
sender
<chr>
receiver
<chr>
type
<chr>
subject
<chr>
28666	david.w.delainey@enron.com	jeff.skilling@enron.com	TO	More UC/CSU Info	
60834	david.w.delainey@enron.com	jeff.skilling@enron.com	TO	More UC/CSU Info	
60835	jeffrey.a.shankman@enron.com	jeff.skilling@enron.com	TO	FW: Please Read - Argentina/Brazil Alert	
60836	jeffrey.a.shankman@enron.com	jeff.skilling@enron.com	TO		
25327	steven.j.kean@enron.com	jeff.skilling@enron.com	TO	California Update--0717.01	
60837	jeffrey.a.shankman@enron.com	jeff.skilling@enron.com	TO	FW: Brazil/Argentina Update]

```{r}

Para profundizar en nuestro análisis, exploraremos las interacciones de Jeff Skilling durante este intervalo de tiempo específico, centrándonos en su red de contactos más cercana. El objetivo es comprender en profundidad cuántas personas estuvieron en contacto con Skilling dentro de su círculo más próximo durante este período de relevancia. Este enfoque nos permitirá revelar no solo la extensión de sus conexiones, sino también la naturaleza y el alcance de sus interacciones dentro de su red profesional.

```{r}
neighborhood.size(network.full,
1,
V(network.full)$lastName == "Skilling")
```
##[1] 34

```{r}
neighborhood.size(network.full,
2,
V(network.full)$lastName == "Skilling")
```
##[1] 140

```{r}
neighborhood.size(network.full,
3,
V(network.full)$lastName == "Skilling")
```
##[1] 146

Como podemos observar en los resultados anteriores, Jeff Skilling tuvo un total de 34 contactos directos, 140 contactos de segundo grado y 146 contactos de tercer grado después del 1 de julio de 2001. Esto demuestra la extensión de su red de contactos y la amplitud de sus interacciones profesionales durante este período de tiempo específico.

```{r}
view(mails.skilling)
```

Ahora podemos observar la red de contactos de Jeff Skilling durante este intervalo de tiempo específico. La visualización de la red nos permite identificar claramente a los contactos directos, de segundo y tercer grado de Skilling, lo que nos ayuda a comprender mejor la estructura y la dinámica de su red profesional durante este período de relevancia.

```{r}
all_contacts <- c(unique(mails.skilling$sender[mails.skilling$sender != "jeff.skilling@enron.com"]), unique(mails.skilling$receiver[mails.skilling$receiver != "jeff.skilling@enron.com"]))
print(all_contacts)
```
#[1] "david.w.delainey@enron.com"   "jeffrey.a.#shankman@enron.com" "steven.j.kean@enron.com"      "jeff.dasovich@enron.com"     
#[5] "stanley.horton@enron.com"     "greg.whalley@enron.com"       "phillip.k.ellen@enron.com"    "sally.beck@enron.com"        
#[9] "rick.buy@enron.com"           "david.w.delainey@enron.com"   "james.derrick@enron.com"      "e..haedicke@enron.com"       
#[13] "stanley.horton@enron.com"     "j.kaminski@enron.com"         "steven.j.kean@enron.com"      "louise.kitchen@enron.com"    
#[17] "danny.mccarty@enron.com"      "jeffrey.a.shankman@enron.com" "richard.shapiro@enron.com"    "greg.whalley@enron.com"      
#[21] "kenneth.lay@enron.com"        "shelley.corman@enron.com"     "drew.fossum@enron.com"        "steven.harris@enron.com"     
#[25] "kevin.m.presto@enron.com"     "james.d.steffes@enron.com"    "barry.tycholiz@enron.com"     "andy.zipper@enron.com" 


```{r}
contador_receiver <- mails.skilling %>%
count(receiver) %>%
rename(name = receiver, counts = n)
contador_receiver <- data.frame(name = all_contacts) %>%
left_join(contador_receiver, by = "name") %>%
replace_na(list(counts = 0))
print(contador_receiver)
```
#Recibidos
name
<chr>
counts
<int>
david.w.delainey@enron.com	4			
jeffrey.a.shankman@enron.com	4			
steven.j.kean@enron.com	4			
jeff.dasovich@enron.com	0			
stanley.horton@enron.com	4			
greg.whalley@enron.com	4			
phillip.k.ellen@enron.com	4			
sally.beck@enron.com	4			
rick.buy@enron.com	4			
david.w.delainey@enron.com	4			
james.derrick@enron.com	4			
e..haedicke@enron.com	4			
stanley.horton@enron.com	4			
j.kaminski@enron.com	4			
steven.j.kean@enron.com	4			
louise.kitchen@enron.com	4			
danny.mccarty@enron.com	4			
jeffrey.a.shankman@enron.com	4			
richard.shapiro@enron.com	4			
greg.whalley@enron.com	4			
kenneth.lay@enron.com	8			
shelley.corman@enron.com	1			
drew.fossum@enron.com	1			
steven.harris@enron.com	1			
kevin.m.presto@enron.com	1			
james.d.steffes@enron.com	1			
barry.tycholiz@enron.com	1			
andy.zipper@enron.com	1	

#Enviados
```{r}
name_counts_sender <- mails.delainey %>%
count(sender) %>%
rename(name = sender, counts = n)
name_counts_sender <- data.frame(name = all_names) %>%
left_join(name_counts_sender, by = "name") %>%
replace_na(list(counts = 0))
print(name_counts_sender)
```
#Todos juntos

```{r}
all_counts <- merge(name_counts_sender, name_counts_receiver, by = "name", all = TRUE)
all_counts$total_counts <- rowSums(all_counts[, c("counts.x", "counts.y")], na.rm = TRUE)

all_counts <- all_counts[order(all_counts$total_counts, decreasing = TRUE), ]
distinct_counts <- all_counts %>%
distinct(name, .keep_all = TRUE)
print(distinct_counts)
```

```{r}

stanley.horton@enron.com	5	4	9	
steven.j.kean@enron.com	5	4	9	
jeffrey.a.shankman@enron.com	4	4	8	
kenneth.lay@enron.com	0	8	8	
david.w.delainey@enron.com	2	4	6	
greg.whalley@enron.com	1	4	5	
danny.mccarty@enron.com	0	4	4	
e..haedicke@enron.com	0	4	4	
j.kaminski@enron.com	0	4	4	
james.derrick@enron.com	0	4	4	

En la tabla anterior, podemos observar que Stanley Horton y Steven Kean tuvieron el mayor número de interacciones con Jeff Skilling durante este período de tiempo específico, con un total de 9 interacciones cada uno. Jeff Shankman y Kenneth Lay también tuvieron un número significativo de interacciones con Skilling, con un total de 8 interacciones cada uno. David Delainey, Greg Whalley, Danny McCarty, E. Haedicke, J. Kaminski y James Derrick tuvieron un número menor de interacciones con Skilling, con un total de 6, 5, 4, 4, 4 y 4 interacciones respectivamente.

Esto sugiere que Stanley Horton y Steven Kean tuvieron una relación más estrecha con Jeff Skilling durante este período de tiempo específico, lo que indica la importancia de estas interacciones en el contexto de la red profesional de Skilling.

Se comunicaba con frecuencia con Stanley Hotyon que era el CEO de Enron, Steven Kean que era el COO de Enron, Jeff Shankman que era el CFO de Enron y Kenneth Lay que era el CEO de Enron. Estas personas ocupaban puestos de alto nivel en la empresa y tenían una relación cercana con Skilling, lo que sugiere que tenían un papel importante en la red profesional de Skilling durante este período de tiempo específico.

Ahora vamos a analizar los correos electrónicos que Jeff Skilling envió y recibió durante este período de tiempo específico, centrándonos en los temas y las discusiones clave que tuvieron lugar en su red profesional.



```{r}
mails.skilling[rownames(mails.skilling) == 38772,]
```
Description:df [1 × 7]
 
 
sender
<chr>
receiver
<chr>
type
<chr>
subject
<chr>
60836	jeffrey.a.shankman@enron.com	jeff.skilling@enron.com	TO		
1 row | 1-5 of 7 columns
body
<chr>
Hi Jeff, hope the weekend was great.With all the management and business changes in the organization the last few months, it might not be a bad idea to get everyone (say OOC at business units and higher, or like the old EI/ENA MD meeting in C
```{r}
#sender  jeffrey.a.shankman@enron.com	
#receiver jeff.skilling@enron.com
#body Hi Jeff, hope the weekend was great.With all the management and business changes in the organization the last few months, it might not be a bad idea to get everyone (say OOC at business units and higher, or like the old EI/ENA MD meeting in C...

El correo electrónico anterior fue enviado por Jeff Shankman a Jeff Skilling y es un ejemplo de las interacciones que tuvieron lugar en la red profesional de Skilling durante este período de tiempo específico. En este correo electrónico, Shankman menciona los cambios de gestión y de negocio en la organización en los últimos meses y sugiere la posibilidad de organizar una reunión para discutir estos cambios. Este correo electrónico revela la importancia de la comunicación y la colaboración en la red profesional de Skilling.

```{r}
mails.skilling[rownames(mails.skilling) == 60835,]
```
#send stanley.horton@enron.com
#receive	jeff.skilling@enron.com
subject ETS BUSINESS REVIEW
#body Jeff:It has been awhile since we did a formal business review with you. Would you like to schedule a 2-hour review? I think we could cover the pipes, PGE, Wessex, Wind and EO

El correo electrónico anterior fue enviado por Stanley Horton a Jeff Skilling y es otro ejemplo de las interacciones que tuvieron lugar en la red profesional de Skilling durante este período de tiempo específico. En este correo electrónico, Horton menciona la posibilidad de organizar una revisión formal del negocio con Skilling y sugiere discutir varios temas relacionados con Enron Energy Services. Este correo electrónico destaca la importancia de la colaboración y la comunicación en la red profesional de Skilling.
sender
All
receiver
All
type
All
subject
All
body
All
date
All
date.R
28666
david.w.delainey@enron.com
jeff.skilling@enron.com
TO


los 5 emails mas destacables son los que tienen id 60876, 60877, 60878, 60879, 60880 porque son los que tienen mas informacion y son los que tienen mas destinatarios. y los mas polemicos son 


```{r}
mails.skilling[rownames(mails.skilling) == 60844,]
```
#sender stanley.horton@enron.com
#receiver jeff.skilling@enron.com
#subject ETS Business review 
#body Jeff:It has been awhile since we did a formal business review with you.  Would you like to schedule a 2-hour review?  I think we could cover the pipes, PGE, Wessex, Wind and EOTT during that period.  It would depend on the level of detail you wanted.Just let me know.Stan

En este correo se menciona que no se ha hecho una revisión formal de negocios con Jeff Skilling y se le pregunta si le gustaría programar una revisión de 2 horas. Se menciona que se podrían cubrir los temas de pipes, PGE, Wessex, Wind y EOTT durante ese período. Se menciona que dependería del nivel de detalle que quisiera Jeff Skilling. 

Llegamos a la conclusion de que los correos mas destacables son los que tienen mas informacion y son los que tienen mas destinatarios. y los mas polemicos son los que tienen menos informacion y son los que tienen menos destinatarios.


```{r}
mails.skilling[rownames(mails.skilling) == 60880,]
```

#sender jeff.skilling@enron.com
#receiver kenneth.lay@enron.com
#subject Please Plan to Attend
#body The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
```

Este correo sugiere que se asista a una presentación de "NUEVA CREACIÓN DE NEGOCIOS" el martes 8 de agosto de 11:30 a 1:30 en la sala de juntas EB50. La presentación será realizada por el profesor de Harvard Business School David A. Garvin. Se menciona que el profesor Garvin está haciendo esta presentación como parte de una asociación de investigación de cinco años entre HBS y Enron titulada "Modern Giants". La presentación fue entregada recientemente en el Seminario de Estrategia Europea de Morgan Stanley, que incluyó a más de 50 CEOs europeos líderes. También se presentó en mayo en HBS como un seminario de gestión avanzada. Partes de la presentación utilizan a Enron como ejemplo. Se menciona que se debe confirmar la asistencia antes del 6 de agosto. Se adjunta el currículum del profesor Garvin. Se menciona que se puede llevar el almuerzo y que se proporcionarán bebidas,lo que sugiere que es un evento formal y no solo una reunión de trabajo. 


```{r}
mails.skilling[rownames(mails.skilling) == 60899,]
```

##sender jeff.dasovich@enron.com
##receiver jeff.skilling@enron.com
##subject Broadband Discussion Opportunity & John Chambers
##body Greetings Jeff:Hope all is well.  I hear that my cameo in the video upstaged the pie....If there s anything I can do to assist on the request from Chambers, just let me know.All the best,Jeff Laura Ipsen <lipsen@cisco.com> 07/18/2001 03:39 PM     To: Jeff.Skilling@enron.com  cc: chambers@cisco.com, Jeff.Dasovich@enron.com  Subject: Broadband Discussion Opportunity & John Chambers Mr. Skilling,I am the Director of Government Affairs at Cisco and work for John Chambers on broadband issues. John is currently one of 8 CEOs on a broadband task force at TechNet that will drive our broadband policy agenda in the US. TechNet (http://www.technet.org) is a 250+ tech CEO-driven policy/political group.We are interested in meeting with the someone in your Broadband Services Group to discuss some of our ideas regarding a business and policy strategy, and hoping you could direct us to the appropriate person(s).Many thanks and regards from John.Laura IpsenDirectorWorldwide Government AffairsCisco Systems, Inc.T:(408) 526-6650F:(408) 853-1598Visit <http://www.cisco.com/gov>Cisco Government Affairs OnlineSign up for <http://www.cisco.com/gov/archive/eupdates/index.html>Cisco s Government Affairs E-Update

Este correo sugiere que se asista a una presentación de "NUEVA CREACIÓN DE NEGOCIOS" el martes 8 de agosto de 11:30 a 1:30 en la sala de juntas EB50. La presentación será realizada por el profesor de Harvard Business School David A. Garvin. Se menciona que el profesor Garvin está haciendo esta presentación como parte de una asociación de investigación de cinco años entre HBS y Enron titulada "Modern Giants". La presentación fue entregada recientemente en el Seminario de Estrategia Europea de Morgan Stanley, que incluyó a más de 50 CEOs europeos líderes. También se presentó en mayo en HBS como un seminario de gestión avanzada. Partes de la presentación utilizan a Enron como ejemplo. Se menciona que se debe confirmar la asistencia antes del 6 de agosto. Se adjunta el currículum del profesor Garvin. Se menciona que se puede llevar el almuerzo y que se proporcionarán bebidas,lo que sugiere que es un evento formal y no solo una reunión de trabajo. 

Gracias a todos estos correos podemos observar que la relacion de Jeff Skilling con sus empleados y colegas era muy buena, ya que siempre estaba al pendiente de las reuniones y eventos que se realizaban en la empresa y siempre estaba dispuesto a ayudar a sus empleados en lo que necesitaran.


#CONCLUSION FINAL

En conclusión, Jeff Skilling era un hombre muy ocupado y siempre estaba al pendiente de las reuniones y eventos que se realizaban en la empresa y siempre estaba dispuesto a ayudar a sus empleados en lo que necesitaran. Se puede observar que Jeff Skilling era una persona muy comprometida con su trabajo y siempre estaba al pendiente de las reuniones y eventos que se realizaban en la empresa.

Podemos observar como se posiciona en la empresa y como se relaciona con sus empleados y colegas, siempre estaba al pendiente de las reuniones y eventos que se realizaban en la empresa y siempre estaba dispuesto a ayudar a sus empleados en lo que necesitaran.
















sender
receiver
type
subject
body
date
date.R
28666
david.w.delainey@enron.com
jeff.skilling@enron.com
TO
More UC/CSU Info
---------------------- Forwarded by David W Delainey/HOU/EES on 07/12/2001 06:36 AM ---------------------------Dan Leff07/11/2001 11:09 PMTo: Marty Sunde/HOU/EES@EES, David W Delainey/HOU/EES@EES, Janet R Dietrich/HOU/EES@EES, Peggy Mahoney/HOU/EES@EES, Elizabeth Tilney/HOU/EES@EES, Vicki Sharp/HOU/EES@EEScc:  Subject: More UC/CSU InfoFYI - Dan   ********************************************************************************************************************************AS uc enron agreement Jul. 11, 2001 Associated Press Newswires Copyright 2001. The Associated Press. All Rights Reserved. University of California, California State University and Enron Reach Settlement Agreement ATTENTION: State, Business editors OAKLAND, Calif., July 11 (AScribe News) -- The University of California, California State University and Enron Energy Services announced today a settlement agreement that will extend their contract for two years and return the two university systems to direct access service from Enron.As part of the agreement, UC and the CSU will drop their lawsuit against Enron and Enron will dismiss its appeal. The two university systems had brought suit in federal court seeking to retain their status as direct access Enron customers after Enron had resourced their power procurement to the Southern California Edison and Pacific Gas&Electric utility companies."We are delighted that our negotiations with Enron have ended with retention of the two university systems as direct access Enron customers," said Joseph P. Mullinix, UC senior vice president for business and finance."The California State University is pleased that we have been able to resolve our differences with Enron and retain our direct access status," said Richard P. West, CSU executive vice chancellor and chief financial officer. "This agreement means considerable savings for the universities and allows us to continue with our energy conservation efforts.""Enron has always been a strong advocate of the benefits of direct access, and we are pleased to be able to extend our contract for two years with UC/CSU," said Marty Sunde, vice chairman, Enron Energy Services."Separately, with the potential elimination of direct access for California customers, Enron is once again directly serving its commercial customers in California, including UC and CSU. As more and more customers in California see the importance of managing their price risk in power markets, commercial energy users have increased interest in our product offerings."The current four-year contract with Enron is scheduled to end on March 31, 2002. The two-year extension, approved in principle, would continue the two university systems as direct access Enron customers until March 31, 2004.Enron and the universities will negotiate price and other terms of the extension between now and Dec. 1, 2001. Both parties have the right to terminate the extension if an agreement is not reached by Dec. 1.The agreement covers all UC campuses and the UC Office of the President, with the exception of UCLA and UC Riverside, which have agreements with their local municipalities. All CSU campuses, except Los Angeles, Northridge, Sacramento and Stanislaus, are part of the Enron contract, as is the CSU Office of the Chancellor.Together, UC and CSU rank as the largest single user of electricity in California. UC s systemwide peak load is 332 megawatts, and CSU s is 117 megawatts. One megawatt powers approximately 1,000 homes.-30-Media Contact: Charles McFadden, University of California, 510-987-9193; charles.mcfadden(at)ucop.eduColleen Bentley-Adler, California State University, 562-951-4801; cbentley-adler(at)calstate.eduPeggy Mahoney, Enron Energy Services, 713-345-7034; pmahoney(at)enron.comAScribe - The Public Interest Newswire / 510-653-9400 $$ APRS0119234206    Copyright ? 2001 Dow Jones Reuters Business Interactive LLC. All Rights Reserved.
2001-07-12 04:36:58
2001-07-12 04:36:58
60834
david.w.delainey@enron.com
jeff.skilling@enron.com
TO
More UC/CSU Info
---------------------- Forwarded by David W Delainey/HOU/EES on 07/12/2001 06:36 AM ---------------------------Dan Leff07/11/2001 11:09 PMTo: Marty Sunde/HOU/EES@EES, David W Delainey/HOU/EES@EES, Janet R Dietrich/HOU/EES@EES, Peggy Mahoney/HOU/EES@EES, Elizabeth Tilney/HOU/EES@EES, Vicki Sharp/HOU/EES@EEScc:  Subject: More UC/CSU InfoFYI - Dan   ********************************************************************************************************************************AS uc enron agreement Jul. 11, 2001 Associated Press Newswires Copyright 2001. The Associated Press. All Rights Reserved. University of California, California State University and Enron Reach Settlement Agreement ATTENTION: State, Business editors OAKLAND, Calif., July 11 (AScribe News) -- The University of California, California State University and Enron Energy Services announced today a settlement agreement that will extend their contract for two years and return the two university systems to direct access service from Enron.As part of the agreement, UC and the CSU will drop their lawsuit against Enron and Enron will dismiss its appeal. The two university systems had brought suit in federal court seeking to retain their status as direct access Enron customers after Enron had resourced their power procurement to the Southern California Edison and Pacific Gas&Electric utility companies."We are delighted that our negotiations with Enron have ended with retention of the two university systems as direct access Enron customers," said Joseph P. Mullinix, UC senior vice president for business and finance."The California State University is pleased that we have been able to resolve our differences with Enron and retain our direct access status," said Richard P. West, CSU executive vice chancellor and chief financial officer. "This agreement means considerable savings for the universities and allows us to continue with our energy conservation efforts.""Enron has always been a strong advocate of the benefits of direct access, and we are pleased to be able to extend our contract for two years with UC/CSU," said Marty Sunde, vice chairman, Enron Energy Services."Separately, with the potential elimination of direct access for California customers, Enron is once again directly serving its commercial customers in California, including UC and CSU. As more and more customers in California see the importance of managing their price risk in power markets, commercial energy users have increased interest in our product offerings."The current four-year contract with Enron is scheduled to end on March 31, 2002. The two-year extension, approved in principle, would continue the two university systems as direct access Enron customers until March 31, 2004.Enron and the universities will negotiate price and other terms of the extension between now and Dec. 1, 2001. Both parties have the right to terminate the extension if an agreement is not reached by Dec. 1.The agreement covers all UC campuses and the UC Office of the President, with the exception of UCLA and UC Riverside, which have agreements with their local municipalities. All CSU campuses, except Los Angeles, Northridge, Sacramento and Stanislaus, are part of the Enron contract, as is the CSU Office of the Chancellor.Together, UC and CSU rank as the largest single user of electricity in California. UC s systemwide peak load is 332 megawatts, and CSU s is 117 megawatts. One megawatt powers approximately 1,000 homes.-30-Media Contact: Charles McFadden, University of California, 510-987-9193; charles.mcfadden(at)ucop.eduColleen Bentley-Adler, California State University, 562-951-4801; cbentley-adler(at)calstate.eduPeggy Mahoney, Enron Energy Services, 713-345-7034; pmahoney(at)enron.comAScribe - The Public Interest Newswire / 510-653-9400 $$ APRS0119234206    Copyright ? 2001 Dow Jones Reuters Business Interactive LLC. All Rights Reserved.
2001-07-12 04:36:58
2001-07-12 04:36:58
60835
jeffrey.a.shankman@enron.com
jeff.skilling@enron.com
TO
FW: Please Read - Argentina/Brazil Alert
Jeff, great job this morning on CNBC.  You were on during our morning crude/products meeting, and the guys were clapping adn doing the old Arsenio Hall thing at the end of the interview... We get these South American reports frequently from our competitive intelligence group, and they are usually very good, politically, and economically.  I can forward them to you, or add you to the distribution list if your interested.  The other Jeff -----Original Message-----From:  Fitzsimmons, Brendan  Sent: Thursday, July 12, 2001 12:36 PMTo: Shankman, Jeffrey A.; Hickerson, Gary; Hannon, Kevin; Kinneman, Jeff; Fiala, Markus; Pizzolato, Paul; Gordon, Michael; Shahi, Pushkar; Stuart III, William; Su, Ellen; Seyfried, Bryan; Wiggs, Brett; Shapiro, Richard; Collonges, Remi; Lee, Derek; Scott, EricCc: Tholan, Scott; Johnston, RobertSubject: Please Read - Argentina/Brazil AlertImportance: HighSensitivity: ConfidentialEverything is fluid at the moment as the Argy conf. call at 9 this morning did not have its intended effect in the markets.  We will update as we get further reports.Brendan Fitzsimmons 713.345.4763Robert Johnston 713.853.9934Here s what we know:INSIDE ARGENTINACavallo is at the edge, very reliably reportedly asking for more power from De La Rua or else.  Markets are already rife with resignation rumorsDomestic political opposition to deficit cut plan is general in Argentina - both in coalition and among opposition, particularly federal v. provincial leaders.  Implementing past cuts still incomplete and willingness to take further local, domestic pain for international markets is very weak.Efforts afoot to try and swap out of short-term Letes (91-day) debt into 1 year, but current conditions (200%+ overnight rates) make it all but impossibleUSTreasury, IMFThere is no sign of any willingness, either at the IMF or Treasury, to do anything for Argentina.  Recent events have only confirmed O Neill s bias against bail-out - he believes it will only be a temporary save and that current problems will be constantly revisited going forwardUSTreasury briefed Fed yesterday on strategy and the bottom line is that Brazil is seen as the best place to make a stand.  There are three arguments behind this:  - First, there is nothing that can reliably save Argy at this point and it is better to husband resources and credibility for use elsewhere if Argy blows-up;  - Second, there are reasons to support Brazil, both for itself and for international contagion concerns: defending Brazil is the best way to quarantine    Argy effects; Brazil has problems internally, but they are seen a more likely to be solved than in Argy aid that they have been exacerbated by Argy;   - Third, defending Brazil is seen as first line defense of Mexico, which is seen as necessary to defend for domestic US political and economic reasons, apart    from the additional international concerns.
2001-07-12 10:55:09
2001-07-12 10:55:09
60836
jeffrey.a.shankman@enron.com
jeff.skilling@enron.com
TO
Hi Jeff, hope the weekend was great.With all the management and business changes in the organization the last few months, it might not be a bad idea to get everyone (say OOC at business units and higher, or like the old EI/ENA MD meeting in Cabo) to an offsite to do business reviews, and do some strategy work.  I certainly wouldn t mind getting away and talking to everyone for a couple of days.Jeff
2001-07-16 10:05:19
2001-07-16 10:05:19
60837
jeffrey.a.shankman@enron.com
jeff.skilling@enron.com
TO
FW: Brazil/Argentina Update
I ll get you guys added to the distribution list.Regards,Jeff -----Original Message-----From:  Fitzsimmons, Brendan  Sent: Monday, July 16, 2001 7:34 PMTo: Shankman, Jeffrey A.; Hickerson, Gary; Hannon, Kevin; Kinneman, Jeff; Fiala, Markus; Pizzolato, Paul; Gordon, Michael; Shahi, Pushkar; Stuart III, William; Su, Ellen; Seyfried, Bryan; Wiggs, Brett; Shapiro, Richard; Collonges, Remi; Lee, Derek; Scott, EricCc: Tholan, Scott; Johnston, RobertSubject: Brazil/Argentina UpdateImportance: HighSensitivity: ConfidentialBRAZILSummaryDefining Brazilian Central Bank s crisis strategy: Facing dilemma of raising rates decisively now or holding fire while waiting on Argy; real can waitIMF and US Treasury: still behind Brazil, behind the scenes, while giving Argentina an opportunity to extricate itself from its current problemsFraga will seek to avoid repeat of last month s mistakes: will not weaken a large rate rise with easing bias again; will not give market new targets to bet against ReportSome arguing a decisive pre-emptive raise - as much as 675bp to 25% by central bank.  In light of the unsettled situation in Argentina and market response to Turkey s 400bp increase (met with continued selling pressure: lira set new lows, bonds down) there may be a move to conserve ammunition in case of a Argentina-induced external shock.  Despite breaking through 2.6BRR/$, the real held and rallied today.  Wait-and-see approach may be chosen on fears of a challenge to a rate rise fueling an upward rate spiral amidst further real depreciation. Central bank not expected to hold interventions to $50 mil./day, particularly if there are further problems in Argy.--------------------------------------------------------------------ARGENTINASummaryDomestic politics continues to drive near-term outcome Cavallo continues to freelance, yielding uncertainty at home and abroadLatest trial balloons focus on prepayment of taxes by banks and privatized companiesLetes swap talk still swirlingNext Letes auction scheduled for next Tuesday (7/24), something has giveEndgames: political compromise and/or Letes swap; tax prepay compromise; Cavallo going to international bankers for  non-confrontational , voluntary restructuring; (if Cavallo goes)  confrontational  default and/or breaking of convertibility pegReportIf resolution does not come before Friday (both political and on Letes swap), expect a very long weekend in Buenos Aires as G-8 leaders meet in Genoa.For now, holding pattern continues as no news is the news.  Latest figures from the central bank (from Thursday) show continued hemorrhaging of foreign reserves.  Capital flight is a key risk factor in the pace of crisis.  FRB debt trading below August 1998 lows.  Despite hue and cry of contagion fears, debt markets outside of Argy still holding up.  Wall St., Pennsylvania Ave. (WH and UST), and IMF still on the same talking points about rhetorical support but no new funds committed.  Even in the currency channel the resilience of the Mexican peso, Chilean peso has been notable.  In Brazil, the real continues to bear the brunt of the assault, but the steepness of the slide has attenuated.  Further evidence of  Brazil-as-defensible  line from the investment community in concert with behind the scenes IMF and governments support. Opposition political (Peronist) leadership and several provincial governors have yet to lend support for President De La Rua s austerity plan.  Despite repeated expectations of agreement from Friday evening on, the Peronist governors have stalled, arguing that they cannot commit until the governing coalition has agreed its plan between its partners.  Alfonsin and leftwing Radicals and junior coalition Frepaso party continue to seek adjustments to the De La Rua-Cavallo austerity package in order to protect their working class and lower middle class bases.  Rhetoric continues to reinforce nationalist populist imperatives.Cavallo s team started spreading the rumor in Argentina that the US was going to help (e.g., telling friends about Cavallo s call to Condi Rice), hoping to spur confidence; instead forced the US to deny it.  Bush s letter to De La Rua offered little more than to say that our prayers are with Argentina.  A national unity government might not include Cavallo if Alfonsin and leftwing of Radical party gains sway in negotiations.The economic team is initiating conversations with the privatized companies (including utilities) and banks in order to push them to pay next year s taxes in advance.  The goal is to collect $2 bil.  This number will cover the estimated deficit for the 2H01 ($1.5bil.).  The banks should advance $1 bil., and the privatized companies another $1 bil.  Repsol has already agreed to advance $150 mil.  Cavallo talked personally with the Presidents of the Spanish companies  (Telef?nica, Telecom, Endesa, etc.) and some Italians.  If successful, these collections will improve this year s numbers and would satisfy Alfonsin and the left wing by allowing normal social spending.Repeat of drip, drip of market information ala May-June  mega-swap .  Goal: swap $4.8 bil. in 91-day debt still to be auctioned during remainder of 2001 into a 1-year bond, a structured product for domestic pension funds, and/or special mutual funds.  Details promised by Wednesday.  Optics tied to political bargaining.  Last Letes auction though fully funded to $800+ mil. goal was not oversubscribed as usual and primary dealers unable to pass it along.
2001-07-17 06:45:36
2001-07-17 06:45:36
25327
steven.j.kean@enron.com
jeff.skilling@enron.com
TO
California Update--0717.01
fyi---------------------- Forwarded by Steven J Kean/NA/Enron on 07/18/2001 07:15 AM ---------------------------From: Jeff Dasovich on 07/17/2001 07:31 PMSent by: Jeff DasovichTo: skean@enron.com, Richard Shapiro/NA/Enron@Enron, James D Steffes/NA/Enron@Enron, Susan J Mara/NA/Enron@ENRON, Harry Kingerski/Enron@EnronXGate, Leslie Lawner/Enron@EnronXGate, Michael Tribolet/ENRON@enronXgate, Kristin Walsh/Enron@EnronXGate, Karen Denne/Enron@EnronXGate, mpalmer@enron.com, Janel Guerrero/Enron@EnronXGate, Paul Kaufman/Enron@EnronXGate, Susan M Landwehr/Enron@EnronXGate, Linda Robertson/NA/Enron@ENRONcc:  Subject: California Update--0717.01What people know:Hertzberg (et al s) bill (82XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow (perhaps beginning at 10 AM) , at which time parties will have a chance to support/oppose and ask for amendments.  Most, including us, oppose unless significantly amended.The Wright (D) -Richman (R) bill (83XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow, at which time parties will have a chance to support/oppose and ask for amendments.  From our perspective, this is the best bill out there yet, though it still has serious problems---it isn t available electronically yet, but should be tomorrow, and I ll distribute then.  The chances of the joint D-R bill being successful are slim, however, since it s up against the Speakers competing bill.  There is talk that the Speaker will try to negotiate with Wright/Richman tonight and include any agreement in his bill (82XX).The original version of the Governor s MOU bill sits in the Senate.  Most believe that Burton will put it up for a vote this week and it will fail.The Senate s version of the MOU (Sher-Peace-Kuehl) (78XX)came out today.  It will likely be heard in the committee tomorrow or the next day.  Notably, it kills Direct Access completely and makes Edison shareholders responsible for that portion of Edison s debt owed to suppliers.  In short, a very bad bill.Burton s 18XX, which would de-link the bond issuance (to pay back the General Fund) from the DWR contracts is likely to pass the Senate tomorrow or the next day.  Many--including Enron--support the bill (though we are supporting it behind the scenes).What people don t know:Whether there s the time or the will in the Assembly and Senate to achieve by Friday a single, comprehensive bill that can be sent to the governor for his signature. Whether the Legislature would postpone its month-long recess if the Legislature hasn t finished a bill by Friday (most folks think they will not postpone).Whether it s true that, irrespective of the energy issue, the Legislature will fail to get the budget completed by Friday and therefore have to postpone their recess anyway, in which case they might continue to work on the energy legislation at the same time.Odds-makers still say it s better than 50-50 that the Legislature does not get the Edison bills done by Friday and leaves on on its 30-day vacation.Best,JeffSacramento is one goofy place.
2001-07-18 00:17:00
2001-07-18 00:17:00
28677
steven.j.kean@enron.com
jeff.skilling@enron.com
TO
California Update--0717.01
fyi---------------------- Forwarded by Steven J Kean/NA/Enron on 07/18/2001 07:15 AM ---------------------------From: Jeff Dasovich on 07/17/2001 07:31 PMSent by: Jeff DasovichTo: skean@enron.com, Richard Shapiro/NA/Enron@Enron, James D Steffes/NA/Enron@Enron, Susan J Mara/NA/Enron@ENRON, Harry Kingerski/Enron@EnronXGate, Leslie Lawner/Enron@EnronXGate, Michael Tribolet/ENRON@enronXgate, Kristin Walsh/Enron@EnronXGate, Karen Denne/Enron@EnronXGate, mpalmer@enron.com, Janel Guerrero/Enron@EnronXGate, Paul Kaufman/Enron@EnronXGate, Susan M Landwehr/Enron@EnronXGate, Linda Robertson/NA/Enron@ENRONcc:  Subject: California Update--0717.01What people know:Hertzberg (et al s) bill (82XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow (perhaps beginning at 10 AM) , at which time parties will have a chance to support/oppose and ask for amendments.  Most, including us, oppose unless significantly amended.The Wright (D) -Richman (R) bill (83XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow, at which time parties will have a chance to support/oppose and ask for amendments.  From our perspective, this is the best bill out there yet, though it still has serious problems---it isn t available electronically yet, but should be tomorrow, and I ll distribute then.  The chances of the joint D-R bill being successful are slim, however, since it s up against the Speakers competing bill.  There is talk that the Speaker will try to negotiate with Wright/Richman tonight and include any agreement in his bill (82XX).The original version of the Governor s MOU bill sits in the Senate.  Most believe that Burton will put it up for a vote this week and it will fail.The Senate s version of the MOU (Sher-Peace-Kuehl) (78XX)came out today.  It will likely be heard in the committee tomorrow or the next day.  Notably, it kills Direct Access completely and makes Edison shareholders responsible for that portion of Edison s debt owed to suppliers.  In short, a very bad bill.Burton s 18XX, which would de-link the bond issuance (to pay back the General Fund) from the DWR contracts is likely to pass the Senate tomorrow or the next day.  Many--including Enron--support the bill (though we are supporting it behind the scenes).What people don t know:Whether there s the time or the will in the Assembly and Senate to achieve by Friday a single, comprehensive bill that can be sent to the governor for his signature. Whether the Legislature would postpone its month-long recess if the Legislature hasn t finished a bill by Friday (most folks think they will not postpone).Whether it s true that, irrespective of the energy issue, the Legislature will fail to get the budget completed by Friday and therefore have to postpone their recess anyway, in which case they might continue to work on the energy legislation at the same time.Odds-makers still say it s better than 50-50 that the Legislature does not get the Edison bills done by Friday and leaves on on its 30-day vacation.Best,JeffSacramento is one goofy place.
2001-07-18 05:17:58
2001-07-18 05:17:58
54992
steven.j.kean@enron.com
jeff.skilling@enron.com
TO
California Update--0717.01
fyi---------------------- Forwarded by Steven J Kean/NA/Enron on 07/18/2001 07:15 AM ---------------------------From: Jeff Dasovich on 07/17/2001 07:31 PMSent by: Jeff DasovichTo: skean@enron.com, Richard Shapiro/NA/Enron@Enron, James D Steffes/NA/Enron@Enron, Susan J Mara/NA/Enron@ENRON, Harry Kingerski/Enron@EnronXGate, Leslie Lawner/Enron@EnronXGate, Michael Tribolet/ENRON@enronXgate, Kristin Walsh/Enron@EnronXGate, Karen Denne/Enron@EnronXGate, mpalmer@enron.com, Janel Guerrero/Enron@EnronXGate, Paul Kaufman/Enron@EnronXGate, Susan M Landwehr/Enron@EnronXGate, Linda Robertson/NA/Enron@ENRONcc:  Subject: California Update--0717.01What people know:Hertzberg (et al s) bill (82XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow (perhaps beginning at 10 AM) , at which time parties will have a chance to support/oppose and ask for amendments.  Most, including us, oppose unless significantly amended.The Wright (D) -Richman (R) bill (83XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow, at which time parties will have a chance to support/oppose and ask for amendments.  From our perspective, this is the best bill out there yet, though it still has serious problems---it isn t available electronically yet, but should be tomorrow, and I ll distribute then.  The chances of the joint D-R bill being successful are slim, however, since it s up against the Speakers competing bill.  There is talk that the Speaker will try to negotiate with Wright/Richman tonight and include any agreement in his bill (82XX).The original version of the Governor s MOU bill sits in the Senate.  Most believe that Burton will put it up for a vote this week and it will fail.The Senate s version of the MOU (Sher-Peace-Kuehl) (78XX)came out today.  It will likely be heard in the committee tomorrow or the next day.  Notably, it kills Direct Access completely and makes Edison shareholders responsible for that portion of Edison s debt owed to suppliers.  In short, a very bad bill.Burton s 18XX, which would de-link the bond issuance (to pay back the General Fund) from the DWR contracts is likely to pass the Senate tomorrow or the next day.  Many--including Enron--support the bill (though we are supporting it behind the scenes).What people don t know:Whether there s the time or the will in the Assembly and Senate to achieve by Friday a single, comprehensive bill that can be sent to the governor for his signature. Whether the Legislature would postpone its month-long recess if the Legislature hasn t finished a bill by Friday (most folks think they will not postpone).Whether it s true that, irrespective of the energy issue, the Legislature will fail to get the budget completed by Friday and therefore have to postpone their recess anyway, in which case they might continue to work on the energy legislation at the same time.Odds-makers still say it s better than 50-50 that the Legislature does not get the Edison bills done by Friday and leaves on on its 30-day vacation.Best,JeffSacramento is one goofy place.
2001-07-18 05:17:58
2001-07-18 05:17:58
60842
steven.j.kean@enron.com
jeff.skilling@enron.com
TO
California Update--0717.01
fyi---------------------- Forwarded by Steven J Kean/NA/Enron on 07/18/2001 07:15 AM ---------------------------From: Jeff Dasovich on 07/17/2001 07:31 PMSent by: Jeff DasovichTo: skean@enron.com, Richard Shapiro/NA/Enron@Enron, James D Steffes/NA/Enron@Enron, Susan J Mara/NA/Enron@ENRON, Harry Kingerski/Enron@EnronXGate, Leslie Lawner/Enron@EnronXGate, Michael Tribolet/ENRON@enronXgate, Kristin Walsh/Enron@EnronXGate, Karen Denne/Enron@EnronXGate, mpalmer@enron.com, Janel Guerrero/Enron@EnronXGate, Paul Kaufman/Enron@EnronXGate, Susan M Landwehr/Enron@EnronXGate, Linda Robertson/NA/Enron@ENRONcc:  Subject: California Update--0717.01What people know:Hertzberg (et al s) bill (82XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow (perhaps beginning at 10 AM) , at which time parties will have a chance to support/oppose and ask for amendments.  Most, including us, oppose unless significantly amended.The Wright (D) -Richman (R) bill (83XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow, at which time parties will have a chance to support/oppose and ask for amendments.  From our perspective, this is the best bill out there yet, though it still has serious problems---it isn t available electronically yet, but should be tomorrow, and I ll distribute then.  The chances of the joint D-R bill being successful are slim, however, since it s up against the Speakers competing bill.  There is talk that the Speaker will try to negotiate with Wright/Richman tonight and include any agreement in his bill (82XX).The original version of the Governor s MOU bill sits in the Senate.  Most believe that Burton will put it up for a vote this week and it will fail.The Senate s version of the MOU (Sher-Peace-Kuehl) (78XX)came out today.  It will likely be heard in the committee tomorrow or the next day.  Notably, it kills Direct Access completely and makes Edison shareholders responsible for that portion of Edison s debt owed to suppliers.  In short, a very bad bill.Burton s 18XX, which would de-link the bond issuance (to pay back the General Fund) from the DWR contracts is likely to pass the Senate tomorrow or the next day.  Many--including Enron--support the bill (though we are supporting it behind the scenes).What people don t know:Whether there s the time or the will in the Assembly and Senate to achieve by Friday a single, comprehensive bill that can be sent to the governor for his signature. Whether the Legislature would postpone its month-long recess if the Legislature hasn t finished a bill by Friday (most folks think they will not postpone).Whether it s true that, irrespective of the energy issue, the Legislature will fail to get the budget completed by Friday and therefore have to postpone their recess anyway, in which case they might continue to work on the energy legislation at the same time.Odds-makers still say it s better than 50-50 that the Legislature does not get the Edison bills done by Friday and leaves on on its 30-day vacation.Best,JeffSacramento is one goofy place.
2001-07-18 05:17:58
2001-07-18 05:17:58
39615
jeff.dasovich@enron.com
jeff.skilling@enron.com
TO
Re: Broadband Discussion Opportunity & John Chambers
Greetings Jeff:Hope all is well.  I hear that my cameo in the video upstaged the pie....If there s anything I can do to assist on the request from Chambers, just let me know.All the best,Jeff Laura Ipsen <lipsen@cisco.com> 07/18/2001 03:39 PM    To: Jeff.Skilling@enron.com  cc: chambers@cisco.com, Jeff.Dasovich@enron.com  Subject: Broadband Discussion Opportunity & John ChambersMr. Skilling,I am the Director of Government Affairs at Cisco and work for John Chambers on broadband issues. John is currently one of 8 CEOs on a broadband task force at TechNet that will drive our broadband policy agenda in the US. TechNet (http://www.technet.org) is a 250+ tech CEO-driven policy/political group.We are interested in meeting with the someone in your Broadband Services Group to discuss some of our ideas regarding a business and policy strategy, and hoping you could direct us to the appropriate person(s).Many thanks and regards from John.Laura IpsenDirectorWorldwide Government AffairsCisco Systems, Inc.T:(408) 526-6650F:(408) 853-1598Visit <http://www.cisco.com/gov>Cisco Government Affairs OnlineSign up for <http://www.cisco.com/gov/archive/eupdates/index.html>Cisco s Government Affairs E-Update
2001-07-18 09:15:00
2001-07-18 09:15:00
23657
steven.j.kean@enron.com
jeff.skilling@enron.com
TO
California Update--0717.01
fyi---------------------- Forwarded by Steven J Kean/NA/Enron on 07/18/2001 07:15 AM ---------------------------From: Jeff Dasovich on 07/17/2001 07:31 PMSent by: Jeff DasovichTo: skean@enron.com, Richard Shapiro/NA/Enron@Enron, James D Steffes/NA/Enron@Enron, Susan J Mara/NA/Enron@ENRON, Harry Kingerski/Enron@EnronXGate, Leslie Lawner/Enron@EnronXGate, Michael Tribolet/ENRON@enronXgate, Kristin Walsh/Enron@EnronXGate, Karen Denne/Enron@EnronXGate, mpalmer@enron.com, Janel Guerrero/Enron@EnronXGate, Paul Kaufman/Enron@EnronXGate, Susan M Landwehr/Enron@EnronXGate, Linda Robertson/NA/Enron@ENRONcc:  Subject: California Update--0717.01What people know:Hertzberg (et al s) bill (82XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow (perhaps beginning at 10 AM) , at which time parties will have a chance to support/oppose and ask for amendments.  Most, including us, oppose unless significantly amended.The Wright (D) -Richman (R) bill (83XX)was heard in an "informational" hearing today and still sits in the Assembly Energy Committee.  It will be heard again tomorrow, at which time parties will have a chance to support/oppose and ask for amendments.  From our perspective, this is the best bill out there yet, though it still has serious problems---it isn t available electronically yet, but should be tomorrow, and I ll distribute then.  The chances of the joint D-R bill being successful are slim, however, since it s up against the Speakers competing bill.  There is talk that the Speaker will try to negotiate with Wright/Richman tonight and include any agreement in his bill (82XX).The original version of the Governor s MOU bill sits in the Senate.  Most believe that Burton will put it up for a vote this week and it will fail.The Senate s version of the MOU (Sher-Peace-Kuehl) (78XX)came out today.  It will likely be heard in the committee tomorrow or the next day.  Notably, it kills Direct Access completely and makes Edison shareholders responsible for that portion of Edison s debt owed to suppliers.  In short, a very bad bill.Burton s 18XX, which would de-link the bond issuance (to pay back the General Fund) from the DWR contracts is likely to pass the Senate tomorrow or the next day.  Many--including Enron--support the bill (though we are supporting it behind the scenes).What people don t know:Whether there s the time or the will in the Assembly and Senate to achieve by Friday a single, comprehensive bill that can be sent to the governor for his signature. Whether the Legislature would postpone its month-long recess if the Legislature hasn t finished a bill by Friday (most folks think they will not postpone).Whether it s true that, irrespective of the energy issue, the Legislature will fail to get the budget completed by Friday and therefore have to postpone their recess anyway, in which case they might continue to work on the energy legislation at the same time.Odds-makers still say it s better than 50-50 that the Legislature does not get the Edison bills done by Friday and leaves on on its 30-day vacation.Best,JeffSacramento is one goofy place.
2001-07-18 10:17:00
2001-07-18 10:17:00
60899
jeff.dasovich@enron.com
jeff.skilling@enron.com
TO
Re: Broadband Discussion Opportunity & John Chambers
Greetings Jeff:Hope all is well.  I hear that my cameo in the video upstaged the pie....If there s anything I can do to assist on the request from Chambers, just let me know.All the best,Jeff Laura Ipsen <lipsen@cisco.com> 07/18/2001 03:39 PM     To: Jeff.Skilling@enron.com  cc: chambers@cisco.com, Jeff.Dasovich@enron.com  Subject: Broadband Discussion Opportunity & John Chambers Mr. Skilling,I am the Director of Government Affairs at Cisco and work for John Chambers on broadband issues. John is currently one of 8 CEOs on a broadband task force at TechNet that will drive our broadband policy agenda in the US. TechNet (http://www.technet.org) is a 250+ tech CEO-driven policy/political group.We are interested in meeting with the someone in your Broadband Services Group to discuss some of our ideas regarding a business and policy strategy, and hoping you could direct us to the appropriate person(s).Many thanks and regards from John.Laura IpsenDirectorWorldwide Government AffairsCisco Systems, Inc.T:(408) 526-6650F:(408) 853-1598Visit <http://www.cisco.com/gov>Cisco Government Affairs OnlineSign up for <http://www.cisco.com/gov/archive/eupdates/index.html>Cisco s Government Affairs E-Update
2001-07-18 14:15:21
2001-07-18 14:15:21
60844
stanley.horton@enron.com
jeff.skilling@enron.com
TO
ETS BUSINESS REVIEW
Jeff:It has been awhile since we did a formal business review with you.  Would you like to schedule a 2-hour review?  I think we could cover the pipes, PGE, Wessex, Wind and EOTT during that period.  It would depend on the level of detail you wanted.Just let me know.Stan
2001-07-30 08:36:20
2001-07-30 08:36:20
60845
jeffrey.a.shankman@enron.com
jeff.skilling@enron.com
TO
Congrats on your engagement!Jeff
2001-07-30 10:09:13
2001-07-30 10:09:13
54750
jeff.skilling@enron.com
phillip.k.ellen@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54751
jeff.skilling@enron.com
sally.beck@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54752
jeff.skilling@enron.com
rick.buy@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54753
jeff.skilling@enron.com
david.w.delainey@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54754
jeff.skilling@enron.com
james.derrick@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54755
jeff.skilling@enron.com
e..haedicke@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54756
jeff.skilling@enron.com
stanley.horton@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54757
jeff.skilling@enron.com
j.kaminski@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54758
jeff.skilling@enron.com
steven.j.kean@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54759
jeff.skilling@enron.com
louise.kitchen@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54760
jeff.skilling@enron.com
danny.mccarty@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54761
jeff.skilling@enron.com
jeffrey.a.shankman@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54762
jeff.skilling@enron.com
richard.shapiro@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54763
jeff.skilling@enron.com
greg.whalley@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54764
jeff.skilling@enron.com
kenneth.lay@enron.com
CC
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
54765
jeff.skilling@enron.com
kenneth.lay@enron.com
BCC
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55386
jeff.skilling@enron.com
phillip.k.ellen@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55387
jeff.skilling@enron.com
sally.beck@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55388
jeff.skilling@enron.com
rick.buy@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55389
jeff.skilling@enron.com
david.w.delainey@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55390
jeff.skilling@enron.com
james.derrick@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55391
jeff.skilling@enron.com
e..haedicke@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55392
jeff.skilling@enron.com
stanley.horton@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55393
jeff.skilling@enron.com
j.kaminski@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55394
jeff.skilling@enron.com
steven.j.kean@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55395
jeff.skilling@enron.com
louise.kitchen@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55396
jeff.skilling@enron.com
danny.mccarty@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55397
jeff.skilling@enron.com
jeffrey.a.shankman@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55398
jeff.skilling@enron.com
richard.shapiro@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55399
jeff.skilling@enron.com
greg.whalley@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55400
jeff.skilling@enron.com
kenneth.lay@enron.com
CC
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
55401
jeff.skilling@enron.com
kenneth.lay@enron.com
BCC
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60850
jeff.skilling@enron.com
phillip.k.ellen@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60851
jeff.skilling@enron.com
sally.beck@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60852
jeff.skilling@enron.com
rick.buy@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60853
jeff.skilling@enron.com
david.w.delainey@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60854
jeff.skilling@enron.com
james.derrick@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60855
jeff.skilling@enron.com
e..haedicke@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60856
jeff.skilling@enron.com
stanley.horton@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60857
jeff.skilling@enron.com
j.kaminski@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60858
jeff.skilling@enron.com
steven.j.kean@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60859
jeff.skilling@enron.com
louise.kitchen@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60860
jeff.skilling@enron.com
danny.mccarty@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60861
jeff.skilling@enron.com
jeffrey.a.shankman@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60862
jeff.skilling@enron.com
richard.shapiro@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60863
jeff.skilling@enron.com
greg.whalley@enron.com
TO
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60864
jeff.skilling@enron.com
kenneth.lay@enron.com
CC
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60865
jeff.skilling@enron.com
kenneth.lay@enron.com
BCC
Please Plan to Attend
 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-01 12:25:58
2001-08-01 12:25:58
60866
jeff.skilling@enron.com
phillip.k.ellen@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60867
jeff.skilling@enron.com
sally.beck@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60868
jeff.skilling@enron.com
rick.buy@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60869
jeff.skilling@enron.com
david.w.delainey@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60870
jeff.skilling@enron.com
james.derrick@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60871
jeff.skilling@enron.com
e..haedicke@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60872
jeff.skilling@enron.com
stanley.horton@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60873
jeff.skilling@enron.com
j.kaminski@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60874
jeff.skilling@enron.com
steven.j.kean@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60875
jeff.skilling@enron.com
louise.kitchen@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60876
jeff.skilling@enron.com
danny.mccarty@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60877
jeff.skilling@enron.com
jeffrey.a.shankman@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60878
jeff.skilling@enron.com
richard.shapiro@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60879
jeff.skilling@enron.com
greg.whalley@enron.com
TO
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60880
jeff.skilling@enron.com
kenneth.lay@enron.com
CC
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
60881
jeff.skilling@enron.com
kenneth.lay@enron.com
BCC
RE: Please Plan to Attend
The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
2001-08-01 14:01:10
2001-08-01 14:01:10
28630
greg.whalley@enron.com
jeff.skilling@enron.com
TO
RE: Please Plan to Attend
i am in london next week.  are you using the video conference for this meet=ing?=20-----Original Message-----=20From: Skilling, Jeff=20Sent: Wed 8/1/2001 3:21 PM=20To: Skilling, Jeff; Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, =David; Bergsieker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ;= Butts, Bob; Buy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwe=ll, Wes; Cordes, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David;= Derrick Jr., James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith;= Donahue, Jeff;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fast=ow, Andrew; Frevert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan,= Ben; Haedicke, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hic=kerson, Gary; Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski,= Vince J; Kean, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Ko=pper, Michael; Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Ma=rtin, Amanda; McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, =Mark;  kristina_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, =Julia; Piper, Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ke=n; Rieker, Paula; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sha=rp, Vicki; Shelby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; =Stram, Bruce; Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, Geo=rge; Whaley, David; Whalley, Greg; Yeager, Scott=20Cc: Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu ;=  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, Malak=20Subject: RE: Please Plan to AttendThe actual date is Wednesday, August 8.=20 -----Original Message-----=20From:   Skilling, Jeff =20Sent:   Wednesday, August 01, 2001 2:26 PM=20To:     Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergs=ieker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob;= Buy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Cor=des, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr.=, James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Je=ff;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; =Frevert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedic=ke, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary=; Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Ke=an, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michae=l; Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda=; McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  krist=ina_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper=, Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, P=aula; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; S=helby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce=; Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley,= David; Whalley, Greg; Yeager, ScottCc:     Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.e=du ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:        Please Plan to Attend=20 =20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th                                                 *Resume att=ached=20Christie Patrick: 3-6117=20Feel free to bring your lunch; drinks will be provided=20 << File: garvinbio.doc >>
2001-08-01 21:50:59
2001-08-01 21:50:59
59136
stanley.horton@enron.com
jeff.skilling@enron.com
CC
Net Variable Power Cost
Someone from Rick Buy s Risk Management group will be calling you to schedule a visit to discuss with your team the procedures and strategy PGE has in place to manage the variability of our net variable power costs.  Since Rick s group is  a corporate Risk Management group there should be no market affiliate issues.  The visit will enable us Houstonians to get even a better handle on the risk profile of your business given the tremendous volatility we have experienced in power prices.Let me know if you have any questions.  Thanks in advance for your cooperation.
2001-08-02 09:38:20
2001-08-02 09:38:20
59138
stanley.horton@enron.com
jeff.skilling@enron.com
BCC
Net Variable Power Cost
Someone from Rick Buy s Risk Management group will be calling you to schedule a visit to discuss with your team the procedures and strategy PGE has in place to manage the variability of our net variable power costs.  Since Rick s group is  a corporate Risk Management group there should be no market affiliate issues.  The visit will enable us Houstonians to get even a better handle on the risk profile of your business given the tremendous volatility we have experienced in power prices.Let me know if you have any questions.  Thanks in advance for your cooperation.
2001-08-02 09:38:20
2001-08-02 09:38:20
60846
stanley.horton@enron.com
jeff.skilling@enron.com
CC
Net Variable Power Cost
Someone from Rick Buy s Risk Management group will be calling you to schedule a visit to discuss with your team the procedures and strategy PGE has in place to manage the variability of our net variable power costs.  Since Rick s group is  a corporate Risk Management group there should be no market affiliate issues.  The visit will enable us Houstonians to get even a better handle on the risk profile of your business given the tremendous volatility we have experienced in power prices.Let me know if you have any questions.  Thanks in advance for your cooperation.
2001-08-02 09:38:20
2001-08-02 09:38:20
60848
stanley.horton@enron.com
jeff.skilling@enron.com
BCC
Net Variable Power Cost
Someone from Rick Buy s Risk Management group will be calling you to schedule a visit to discuss with your team the procedures and strategy PGE has in place to manage the variability of our net variable power costs.  Since Rick s group is  a corporate Risk Management group there should be no market affiliate issues.  The visit will enable us Houstonians to get even a better handle on the risk profile of your business given the tremendous volatility we have experienced in power prices.Let me know if you have any questions.  Thanks in advance for your cooperation.
2001-08-02 09:38:20
2001-08-02 09:38:20
60882
jeff.skilling@enron.com
shelley.corman@enron.com
TO
Please Plan to Attend
PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-07 07:20:12
2001-08-07 07:20:12
60883
jeff.skilling@enron.com
drew.fossum@enron.com
TO
Please Plan to Attend
PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-07 07:20:12
2001-08-07 07:20:12
60884
jeff.skilling@enron.com
steven.harris@enron.com
TO
Please Plan to Attend
PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-07 07:20:12
2001-08-07 07:20:12
60885
jeff.skilling@enron.com
kevin.m.presto@enron.com
TO
Please Plan to Attend
PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-07 07:20:12
2001-08-07 07:20:12
60886
jeff.skilling@enron.com
james.d.steffes@enron.com
TO
Please Plan to Attend
PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-07 07:20:12
2001-08-07 07:20:12
60887
jeff.skilling@enron.com
barry.tycholiz@enron.com
TO
Please Plan to Attend
PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-07 07:20:12
2001-08-07 07:20:12
60888
jeff.skilling@enron.com
andy.zipper@enron.com
TO
Please Plan to Attend
PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th *Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
2001-08-07 07:20:12
2001-08-07 07:20:12
Showing 1 to 23 of 90 entries, 7 total columns

Estos son los mails enviado spor jeff skilling en agosto de 2001. 

Podemos ver algunas conversaciones relvevantes, como la de la reunion de negocios el 8 de agosto, o la de la visita de Rick Buy a la oficina de Jeff Skilling, que tuvo bastante escandalo Enron debido a la manipulacion de precios de la energia.

Estos mails concretos son los que se usaron en el juicio de Enron, y se pueden encontrar en la web. Id de los correos son 60877, 60878, 60879, 60880, 60882, 60883, 60884, 60885, 60886, 60887, 60888.

Esta conversacion entre Jeff Skilling y Jefrey Shank,  muestra la manipulacion de precios de la energia, y la visita de Rick Buy a la oficina de Jeff Skilling para discutir la estrategia de PGE para manejar la variabilidad de los costos de energia.
```{r}
mails.skilling[rownames(mails.skilling) == 60877,]
mails.skilling[rownames(mails.skilling) == 60878,]
mails.skilling[rownames(mails.skilling) == 60879,]
mails.skilling[rownames(mails.skilling) == 60880,]
mails.skilling[rownames(mails.skilling) == 60882,]
mails.skilling[rownames(mails.skilling) == 60883,]
mails.skilling[rownames(mails.skilling) == 60884,]
mails.skilling[rownames(mails.skilling) == 60885,]
mails.skilling[rownames(mails.skilling) == 60886,]
mails.skilling[rownames(mails.skilling) == 60887,]
mails.skilling[rownames(mails.skilling) == 60888,]

```

##                        sender                     receiver type
## 60877 jeff.skilling@enron.com jeffrey.a.shankman@enron.com   TO
##                         subject
## 60877 RE: Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   body
## 60877 The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
##                      date              date.R
## 60877 2001-08-01 14:01:10 2001-08-01 14:01:10
mails.skilling[rownames(mails.skilling) == 60878,]
##                        sender                  receiver type
## 60878 jeff.skilling@enron.com richard.shapiro@enron.com   TO
##                         subject
## 60878 RE: Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60878 The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
##                      date              date.R
## 60878 2001-08-01 14:01:10 2001-08-01 14:01:10
mails.skilling[rownames(mails.skilling) == 60879,]
##                        sender               receiver type
## 60879 jeff.skilling@enron.com greg.whalley@enron.com   TO
##                         subject
## 60879 RE: Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60879 The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
##                      date              date.R
## 60879 2001-08-01 14:01:10 2001-08-01 14:01:10
mails.skilling[rownames(mails.skilling) == 60880,]
##                        sender              receiver type
## 60880 jeff.skilling@enron.com kenneth.lay@enron.com   CC
##                         subject
## 60880 RE: Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60880 The actual date is Wednesday, August 8. -----Original Message-----From: =09Skilling, Jeff =20Sent:=09Wednesday, August 01, 2001 2:26 PMTo:=09Allen, Phillip K.; Bay, Frank; Beck, Sally; Berberian, David; Bergsie=ker, Rick; Blachman, Jeremy;  kathleen.blakenship@enron.com ; Butts, Bob; B=uy, Rick; Carter, Rebecca;  richard.cause4y@enron.com ; Colwell, Wes; Corde=s, Bill; Cumberland, Shawn; Deffner, Joseph; Delainey, David; Derrick Jr., =James; Detmering, Timothy J.; Dimichele, Rich; Dodson, Keith; Donahue, Jeff=;  w.david.curan@enron.com ; Earle, Joseph; Fallon, Jim; Fastow, Andrew; Fr=evert, Mark; Gahn, Scott;  kevin.garlandd@enron.com ; Glisan, Ben; Haedicke=, Mark E.; Hannon, Kevin; Hayes, Robert; Hermann, Robert; Hickerson, Gary; =Horton, Stanley; Humphrey, Gene; Jackson, Charlene; Kaminski, Vince J; Kean=, Steven J.; Kishkill, Joe; Kitchen, Louise; Koenig, Mark; Kopper, Michael;= Lavorato, John; Leff, Dan; Lindholm, Tod A.; Lowry, Paul; Martin, Amanda; =McCarty, Danny; Mcclellan, George; McDonald, Rebecca; Metts, Mark;  kristin=a_mourdaunt@enron.net ; Muench, Gayle; Muller, Mark; Murray, Julia; Piper, =Greg; Prentice, James; Reck, Daniel; Redmond, Brian; Rice, Ken; Rieker, Pau=la; Sera, Sherri; Shankman, Jeffrey A.; Shapiro, Richard; Sharp, Vicki; She=lby, Rex; Sherrick, Jeffrey; Stabler, Frank; Stanley, Brian; Stram, Bruce; =Taylor, Mitch; Tilney, Elizabeth; Walls Jr., Rob; Wasaff, George; Whaley, D=avid; Whalley, Greg; Yeager, ScottCc:=09Lay, Kenneth; Patrick, Christie;  dgarvin@hsb.edu ;  mroberto@hsb.edu= ;  jbower@hsb.edu ;  llevesque@hsb.edu ; Hamed, MalakSubject:=09Please Plan to Attend=20PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Tuesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year researc=h partnership between HBS and Enron entitled  Modern Giants .  The presenta=tion was recently delivered at the Morgan Stanley European Strategy Seminar=, which included over 50 leading European CEOs. It was also presented in Ma=y at HBS as an advanced management seminar. Parts of the presentation use E=nron as an example.=20Rsvp by Aug 6th=09=09=09=09=09=09=09*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided << File: garvinbio.doc >>
##                      date              date.R
## 60880 2001-08-01 14:01:10 2001-08-01 14:01:10
mails.skilling[rownames(mails.skilling) == 60882,]
##                        sender                 receiver type
## 60882 jeff.skilling@enron.com shelley.corman@enron.com   TO
##                     subject
## 60882 Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60882 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th\t\t\t\t\t\t\t*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
##                      date              date.R
## 60882 2001-08-07 07:20:12 2001-08-07 07:20:12
mails.skilling[rownames(mails.skilling) == 60883,]
##                        sender              receiver type               subject
## 60883 jeff.skilling@enron.com drew.fossum@enron.com   TO Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60883 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th\t\t\t\t\t\t\t*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
##                      date              date.R
## 60883 2001-08-07 07:20:12 2001-08-07 07:20:12
mails.skilling[rownames(mails.skilling) == 60884,]
##                        sender                receiver type
## 60884 jeff.skilling@enron.com steven.harris@enron.com   TO
##                     subject
## 60884 Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60884 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th\t\t\t\t\t\t\t*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
##                      date              date.R
## 60884 2001-08-07 07:20:12 2001-08-07 07:20:12
mails.skilling[rownames(mails.skilling) == 60885,]
##                        sender                 receiver type
## 60885 jeff.skilling@enron.com kevin.m.presto@enron.com   TO
##                     subject
## 60885 Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60885 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th\t\t\t\t\t\t\t*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
##                      date              date.R
## 60885 2001-08-07 07:20:12 2001-08-07 07:20:12
mails.skilling[rownames(mails.skilling) == 60886,]
##                        sender                  receiver type
## 60886 jeff.skilling@enron.com james.d.steffes@enron.com   TO
##                     subject
## 60886 Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60886 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th\t\t\t\t\t\t\t*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
##                      date              date.R
## 60886 2001-08-07 07:20:12 2001-08-07 07:20:12
mails.skilling[rownames(mails.skilling) == 60887,]
##                        sender                 receiver type
## 60887 jeff.skilling@enron.com barry.tycholiz@enron.com   TO
##                     subject
## 60887 Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60887 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th\t\t\t\t\t\t\t*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
##                      date              date.R
## 60887 2001-08-07 07:20:12 2001-08-07 07:20:12
mails.skilling[rownames(mails.skilling) == 60888,]
##                        sender              receiver type               subject
## 60888 jeff.skilling@enron.com andy.zipper@enron.com   TO Please Plan to Attend
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    body
## 60888 PLEASE  PLAN  TO  ATTEND"NEW  BUSINESS  CREATION"Wednesday, August 8th11:30-1:30Board RoomEB50Presented byHarvard Business School ProfessorDavid A. Garvin*Professor Garvin is making this presentation as part of a five-year research partnership between HBS and Enron entitled  Modern Giants .  The presentation was recently delivered at the Morgan Stanley European Strategy Seminar, which included over 50 leading European CEOs. It was also presented in May at HBS as an advanced management seminar. Parts of the presentation use Enron as an example. Rsvp by Aug 6th\t\t\t\t\t\t\t*Resume attachedChristie Patrick: 3-6117Feel free to bring your lunch; drinks will be provided
##                      date              date.R
## 60888 2001-08-07 07:20:12 2001-08-07 07:20:12

```
Estos 11 correos son los que se usaron en el juicio de Enron, y se pueden encontrar en la web. Muestran la manipulacion de precios de la energia, y la visita de Rick Buy a la oficina de Jeff Skilling para discutir la estrategia de PGE para manejar la variabilidad de los costos de energia.

Con esto podemos ver que los correos de Jeff Skilling son bastante relevantes para el caso de Enron, y que se usaron en el juicio. 

Skilling, 52, fue condenado por 19 de los 28 cargos de los que fue acusado: conspiración, 12 cargos de fraude de valores, un cargo de abuso de información privilegiada y cinco cargos de realización de declaraciones falsas a auditores. Skilling fue absuelto de nueve cargos de abuso de información privilegiada.

Por un lado, vemos como la compañía creó una serie de vehículos financieros para poder esconder sus pérdidas y mantener al alza su precio de cotización en la bolsa, lo cual le permitía vender sus acciones sobrevaloradas obteniendo rentabilidades máximas.

Por otro lado, vemos como la compañía manipuló los precios de la energía en California, lo cual le permitió obtener beneficios adicionales y los correos muestran como Jeff Skilling estaba al tanto de estas prácticas, y como las promovía. 
```{r}