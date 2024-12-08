# PARP2024Z - Zespół 4

## Opis
Repozytorium zawiera rozwiązania 3 zadań projektowych z przedmiotu [Paradygmaty Programowania](https://usosweb.usos.pw.edu.pl/kontroler.php?_action=katalog2/przedmioty/pokazPrzedmiot&kod=103A-INxxx-ISP-PARP).

## Autorzy
- Mateusz Lewko
- Jerzy Muszyński ()
- Sebastian Wojciechowski

## Struktura Repozytorium
- `prolog/` - rozwiązanie zadania w języku prolog
- `haskell/` - rozwiązanie zadania w języku haskell
- `smalltalk/` - rozwiązanie zadania w języku smalltalk

## Wymagania
- swi-prolog
- haskell-platform
- gnu-smalltalk

## Instalacja
### Debian-based
#### 1. Sklonuj repozytorium:
```bash
git clone https://gitlab-stud.elka.pw.edu.pl/parp-z4/parp2024z.git
```
#### 2. Zainstaluj wymagane zależności:

    ```bash
    cd parp2024z

    sudo apt-get update
    sudo apt-get install -y swi-prolog
    sudo apt-get install -y haskell-platform
    sudo apt-get install -y gnu-smalltalk
    ```
### Nix
- Jeśli ma się skonfigurowane SSH w GitLabie
    ```bash
    # Uruchom grę w Prologu
    nix run git+ssh://git@gitlab-stud.elka.pw.edu.pl/parp-z4/parp2024z#prolog
    # Uruchom grę w Haskellu
    nix run git+ssh://git@gitlab-stud.elka.pw.edu.pl/parp-z4/parp2024z#haskell
    # Uruchom grę w Smalltalku # (w przyszłości)
    nix run git+ssh://git@gitlab-stud.elka.pw.edu.pl/parp-z4/parp2024z#smalltalk
    ```
- W przeciwnym wypadku
    ```bash
    git clone https://gitlab-stud.elka.pw.edu.pl/parp-z4/parp2024z.git
    cd parp2024z
    nix run .#prolog
    nix run .#haskell
    nix run .#smalltalk
    ```
