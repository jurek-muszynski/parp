# Dokumentacja do Projektu "Ucieczka z Choroszczy"

Gra w języku Prolog, której celem jest ucieczka z fikcyjnego szpitala psychiatrycznego w Choroszczy i dotarcie do najbliższego miasta, Białegostoku. Gracz eksploruje różne pomieszczenia, rozwiązuje zagadki, zbiera przedmioty, wchodzi w interakcje z NPC (pacjentami/lekarzami).

---

## Spis Treści
1. [Założenia przed a aktualny stan projektu](#założenia-przed-a-aktualny-stan-projektu)
1. [Instrukcja Obsługi](#instrukcja-obsługi)
1. [Mechanika Gry](#mechanika-gry)  
2. [Lokacje](#lokacje)  
3. [Postacie NPC](#postacie-npc)  
4. [System Przedmiotów](#system-przedmiotów)  
5. [Autorzy](#autorzy) 

## Założenia przed a aktualny stan projektu

Motywy: 

* Zamknięte drzwi, które otwiera się przy pomocy karty/kodu dostępowego, który musi być odnaleziony gdzieś na mapie :white_check_mark: 
* Ograniczony czas, gracz może mieć określoną liczbe =\> każda akcja zużywa jedną turę, czas mija to gracz przegrywa :x: 
* Rozmowa z pacjentem/lekarzem =\> postacie dostarczą graczowi pewne wskazówki, jedynie wtedy gdy ma on wcześniej konkretne przedmioty (artefakty, dokumentacje, wydruk z badań) :white_check_mark: 

Lokacje: 

* Recepcja =\> jeden z początkowych punktów gry :white_check_mark: 
* Korytarz =\> zawiera drzwi do różnych pomieszczeń, w których znajdowałyby się artefakty/kartoteki pacjentów itd. :white_check_mark: 
* Sala zabiegowa =\> jedna ze "specjalnych" sal, ma zamknięte drzwi, a za nimi znajdować się mogą pewne zagadki do rozwiązania, kluczowe do pełnej realizacji gry :white_check_mark: 
* Piwnica =\> jest tam bilet na autobus do Białegostoku, ostatni element konieczny do ucieczki :white_check_mark: / :x: 
* Przystanek =\> koniec gry :white_check_mark: 

## Instrukcja obsługi


### 1. Uruchomienie gry
Aby uruchomić grę, w wersji prolog, w terminalu, wykonaj następujące kroki:
```bash
git clone https://gitlab-stud.elka.pw.edu.pl/parp-z4/parp2024z.git
cd parp2024z/prolog
swipl
```

```prolog
[adventure].
start.
```

1. Przejdź do katalogu prolog
```bash
cd prolog
```
2. Otwórz SWI-Prolog:
```bash
swipl
```
3. Załaduj plik z grą:
```prolog
[adventure].
```
4. Rozpocznij rozgrywkę:
```prolog
start.
```
Wszystkie dostępne akcje i komendy są opisane w sekcji **Instructions** w grze
```prolog
instructions.
```
## Motywy w grze

- **Zamknięte drzwi:**  
  W grze występują drzwi, które można otworzyć bez użycia klucza oraz które wymagają kluczy odnalezionych w różnych lokacjach.

- **Interakcje z NPC:**  
  Postacie niezależne, takie jak recepcjonistka i lekarz, mogą udzielać wskazówek lub pomagać graczowi, pod warunkiem, że posiada on wymagane przedmioty.


## Lokacje

<img src="prolog/figures/diagram.png" height="500px" align="center"/>

### 1. Cela
- **Opis:** Gracz budzi się w zamkniętej, wyściełanej celi bez wspomnień o tym, jak się tu znalazł.  
- **Zawartość:** Klucz potrzebny do otwarcia drzwi od celi.  
- **Wyjście:** Drzwi prowadzące do recepcji (zamknięte, wymagają klucza). 

### 2. Recepcja  
- **Opis:** Recepcja szpitala psychiatrycznego.  
- **Zawartość:**
  - Recepcjonistka z którą możemy nawiązać dialog
  - Wejścia do innych pomieszczeń:  
    - **Łazienka**: Można tam znaleźć dodatkowy przedmiot lub wskazówkę.  
    - **Drzwi wyjściowe**: Zamknięte na klucz.
    - **Korytarz**: Prowadzi do dalszych lokacji.  

### 3. Korytarz  
- **Opis:** Korytarz prowadzący do różnych pomieszczeń.  
- **Zawartość:** Drzwi do dwóch sal zabiegowych, do piwnicy oraz możliwość powrotu do recepcji

### 4. Sala Zabiegowa 1  
- **Opis:** Pierwsza sala zabiegowa, w której można znaleźć wskazówki dotyczące dalszej drogi.  
- **Zawartość:**  
  - Wskazówka, która może być konieczna do rozwiązania zagadki w innej lokacji lub interakcji z NPC.  

### 5. Sala Zabiegowa 2  
- **Opis:** Druga sala zabiegowa, w której można znaleźć kluczowy przedmiot i porozmawiać z lekarzem.  
- **Zawartość:**  
  - Dokument do wypisania ze szpitala (potrzebny do otwarcia dalszej drogi).  
  - NPC (lekarz): Można z nim porozmawiać, ale tylko jeśli gracz posiada odpowiednie przedmioty.

### 6. Piwnica  
- **Opis:** Ostateczny cel gracza w szpitalu. Znajduje się tu bilet na autobus do Białegostoku.  

### 7. Wyjście  
- **Opis:** Końcowy punkt gry, gdzie gracz może udać się do przystanku autobusowego lub wrócić do recepcji.


## Postacie NPC

### Lekarz i Recepcjonistka  
NPC dostarczają graczowi wskazówki, ale tylko wtedy, gdy spełnione są określone warunki (np. posiadanie odpowiedniego przedmiotu).  

#### Przykładowy dialog:
```prolog

person_descr(doctor) :-
    slow_print('The doctor looks up from his desk and says, "Hi I how can I help you"'), nl.

dialog_line(doctor, 'Ask for discharge.', ask(doctor, discharge)).

ask(doctor, discharge) :-
    \+ holding(discharge_form),
    slow_print('The doctor looks at you and says, "I cannot discharge you until you have the proper paperwork. Please find your discharge form."'), nl,
    slow_print('The doctor has just sent you back to your padded cell and collected all of your belongings.'), nl,
    retractall(holding(_)),
    retract(i_am_at(exam_room1)),
    assert(i_am_at(padded_cell)),
    retract(in_dialog(doctor)),
    !, nl.
```

## System Przedmiotów

1. padded_cell_key - potrzebny do wyjścia z celi
2. exit_key - potrzebny do wyjścia ze szpitala
3. patient_file - dokumenty z informacjami o pacjencie
4. discharge_form - dokument potrzebny do wypisania ze szpitala, należy go zdobyć przed rozmową z lekarzem
5. set_of_notes - notatki zawierające wskazówki do kodu potrzebnego do wejścia do piwnicy


## Autorzy
- Sebastian Wojciechowski
- Jerzy Muszyński 
- Mateusz Lewko
