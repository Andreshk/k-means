# k-means clusterization

Реализация на прост, но мощен алгоритъм за клъстеризация на 2D точки, представени като наредени двойки `(Double,Double)`. Използвани са прости и оптимални за повечето цели структури от данни, които се обхождат итеративно, избягва се търсене в големи списъци, някои изчисления са стриктни и т.н. Допълнително се измерва общото време за работата на програмата (вкл. IO операциите, парсването на входа и форматирането на изхода), както и след колко итерации е приключил алгоритъма. Накрая се изчислява и within-cluster sum of squares - сумата от квадратите на разстоянията между всяка точка от даден клъстер и центъра на клъстера (общо за всички клъстери). Това количество на практика се опитва алгоритъмът да минимизира, но в зависимост от избраните начални центрове може да не достигне глобален минимум. За удобство достигнатия минимум се изписва на стандартния изход, заедно с броя итерации и времето за работа на програмата.

Необходим е пакетът [`clock`](https://hackage.haskell.org/package/clock) за точно отмерване на времето (без него кодът, естествено, не се компилира).

Може би най-съществената хитринка е във функцията `reClusterize`, която с _едно_ линейно итеративно и стриктно обхождане на пълния списък от точки изчислява за всяка от тях кой е новият ѝ клъстер, и по същото време следи дали някоя си го променя, и връща тази информация като `Bool` в наредена двойка със получения списък от точки. От друга страна, функцията `groupToClusters` може да се подобри значително, тъй като също се извиква по веднъж на всяка итерация, а блести по-скоро с простота, отколкото с ефективност. Има и други функции, които могат да се оптимизират по начини, за които компилаторът просто няма как да се досети, а може и да се изберат различни структури от данни (като вектори), но това са long-distance цели за подобрение. Във всички случаи в този си вид това е proof of concept.

Кодът е предназначен да се компилира до executable, който при стартиране получава три аргумента:
* име на файл в входните данни - текстови файл, в който всеки ред съдържа координатите на една точка, разделени с празно място (вж. [`input1250.txt`](https://github.com/Andreshk/k-means/blob/master/input1250.txt) за пример)
* брой търсени клъстери
* име на файл за изходните данни - резултата от клъстеризирането.

Аргументите не се валидират, следи се единствено техния брой (поне три, всички след третия се игнорират).

Файлът [`input1250.txt`](https://github.com/Andreshk/k-means/blob/master/input1250.txt) служи за примерен вход и съдържа 1249 точки, без никаква особена вътрешна структура (любезно предоставен с цел тестване от @zaki1993). На моя неособен лаптоп клъстеризирането им в 4 клъстера отнема средно ~0,3 секунди, а в 10 клъстера - около 0,7 секунди.
