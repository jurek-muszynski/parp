
# Ucieczka z Choroszczy - Dokumentacja

Gra w języku Haskell, której celem jest ucieczka z fikcyjnego szpitala psychiatrycznego w Choroszczy i dotarcie do najbliższego miasta, Białegostoku. Gracz eksploruje różne pomieszczenia, rozwiązuje zagadki, zbiera przedmioty, wchodzi w interakcje z NPC (pacjentami/lekarzami).

---
## Spis Treści

1. [Stan Pracy](#stan-pracy)
2. [Instrukcja Obsługi](#instrukcja-obsługi)
3. [Mechanika Gry](#mechanika-gry)
4. [Lokacje](#lokacje)
5. [Przedmioty](#przedmioty)
6. [Postacie NPC](#postacie-npc)
7. [Struktura Systemu](#struktura-systemu)
8. [Struktura Kodu](#struktura-kodu)

## Stan Pracy
- [x] implementacja wszystkich pomieszczeń oraz połączen między nimi bazując na projekcie prologowym
- [x] umożliwianie koniecznych interakcji (poruszanie się, podnoszenie przedmiotów, rozmowa z NPC, wpisywanie kodu)
- [x] warunkowe przejścia między pomieszczeniami i zakończenie Rozgrywki
- [ ] implementacja dodatkowych funkcji (przyglądanie się przedmiotom, odkładanie przedmiotów, rozbudowane dialogi, połaczone z przekazywaniem przedmiotów) 

## Instrukcja Obsługi

### Uruchomienie Gry

1. Sklonuj repozytorium:

```bash
git clone https://gitlab-stud.elka.pw.edu.pl/parp-z4/parp2024z.git
cd haskell
```

2. Zbuduj grę za pomocą Cabal:

```bash
cabal build
```

3. Uruchom grę:

```bash
cabal run
```

### Komendy
- **Ruch**:  
  Użyj `n`, `s`, `e`, `w`, aby poruszać się w odpowiednich kierunkach.
- **Interakcje**:  
  Wpisz `a`, `b`, `c` itd., aby wchodzić w interakcje z przedmiotami lub NPC w danej lokacji.
- **Spójrz Wokół**:  
  Wpisz `look`, aby zobaczyć opis aktualnej lokacji.
- **Ekwipunek**:  
  Wpisz `inventory`, aby zobaczyć posiadane przedmioty.
- **Restart Gry**:  
  Wpisz `restart`, aby zrestartować grę.
- **Zakończenie Gry**:  
  Wpisz `quit`, aby zakończyć grę.
- **Pomoc**:  
  Wpisz `help`, aby zobaczyć listę dostępnych komend.

---

## Mechanika Gry

### Ruch
Gracz porusza się między pomieszczeniami połączonymi kierunkami (północ, południe, wschód, zachód). Każda lokacja zawiera opis, przedmioty i wyjścia.

### Interakcje
Interakcje zależą od przedmiotów i NPC w danej lokacji. Niektóre interakcje wymagają posiadania odpowiednich przedmiotów.

---

## Lokacje

<img src="haskell/figures/diagram.png" height="500px" align="center"/>


1. **Cela**:
   - **Opis**: Zamknięta cela, w której gracz rozpoczyna grę.
   - **Zawartość**: Klucz do otwarcia drzwi.
   - **Wyjście**: Prowadzi do recepcji.

2. **Recepcja**:
   - **Opis**: Główne miejsce w szpitalu z dostępem do innych pomieszczeń.
   - **Zawartość**:
     - NPC: Recepcjonistka.
     - Wyjścia do łazienki, korytarzy i drzwi wyjściowych.

3. **Korytarze**:
   - **Opis**: Połączone pomieszczenia prowadzące do różnych lokacji.

4. **Sala Zabiegowa 1**:
   - **Opis**: Zawiera wskazówki potrzebne do rozwiązania zagadek.

5. **Sala Zabiegowa 2**:
   - **Opis**: Zawiera kluczowe przedmioty i NPC.

6. **Piwnica**:
   - **Opis**: Znajduje się tam bilet autobusowy, wymagany do ucieczki.

7. **Wyjście**:
   - **Opis**: Miejsce kończące grę.

---

## Przedmioty

1. **Padded Cell Key**: Otwiera drzwi celi.
2. **Exit Key**: Otwiera drzwi wyjściowe.
3. **Patient File**: Dokumenty z informacjami o pacjencie.
4. **Discharge Form**: Dokument potrzebny do interakcji z lekarzem.
5. **Set of Notes**: Zawiera wskazówki do rozwiązania zagadek.

---

## Postacie NPC

1. **Recepcjonistka**:
   - Pomaga w nawigacji i dostarcza wskazówek.

2. **Lekarz**:
   - Wymaga odpowiedniego formularza do kontynuowania interakcji.

---

## Struktura Systemu

### Ruch
Lokacje są połączone w strukturze grafowej, gdzie każdy pokój posiada listę kierunków i sąsiadujących pomieszczeń.

### Przedmioty
Przedmioty są przechowywane globalnie i przypisane do lokacji przez indeksy.

### NPC
NPC mają zdefiniowane dialogi i interakcje, które zależą od stanu gracza.

---

## Struktura Kodu

### Main.hs
- **Opis**: Główna pętla gry, obsługa wejścia użytkownika i sterowanie rozgrywką.
- **Kluczowe Funkcje**:
  - `main`: Inicjalizuje grę.
  - `gameLoop`: Obsługuje komendy gracza.
  - `describeCurrentLocation`: Wyświetla szczegóły aktualnej lokacji.

### State.hs
- **Opis**: Definiuje struktury danych dla stanu gry.
- **Struktury Danych**:
  - `State`: Stan gry.
  - `Room`: Lokacja w grze.
  - `Item`: Przedmiot.
  - `Person`: NPC.

### Gameplay.hs
- **Opis**: Logika gry.
- **Kluczowe Funkcje**:
  - `move`: Obsługuje ruch pomiędzy lokacjami.
  - `interact`: Obsługuje interakcje z przedmiotami i NPC.

---

## Przykład Rozgrywki

Przykład po wpisaniu `look` w recepcji:

```
You are at the Reception.
It is mostly empty, apart from a few flower pots set against the walls and a desk in the middle.
Behind it sits a middle-aged woman illuminated by the computer screen.

Available directions:
  n - Hallway 1
  s - Hallway 2
  e - Restroom
  w - Exit

You see:
  - Patient File: Dokumentacja pacjenta.
  - Recepcjonistka: Kobieta za biurkiem.
```

## Autorzy
- Sebastian Wojciechowski
- Jerzy Muszyński 
- Mateusz Lewko
