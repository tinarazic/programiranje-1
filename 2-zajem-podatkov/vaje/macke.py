import requests
import re
import string
import csv
import string
import os

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = '2-zajem-podatkov/vaje/cat_data'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'catdata.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # del kode, ki morda sproži napako
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print("Could not access page " + url)
        return " "
        # nadaljujemo s kodo če ni prišlo do napake
    return r.text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(url, directory, filename):
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    text = download_url_to_string(url)
    save_string_to_file(text, directory, filename)
    return None

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    with open(directory + "\" + filename, 'r', encoding='utf8') as vsebina:
        return vsebina.read()

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.

vsebina = read_file_to_string(cat_directory, frontpage_filename)


def page_to_ads(niz):
    '''Split "page" to a list of advertisement blocks.'''
    vzorec = re.compile(
        r'<div class="ad.*?">(.*?)<div class="clear"></div>', 
        re.DOTALL
        )
    oglasi = vzorec.findall(niz)
    return oglasi

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.

oglasi = page_to_ads(vsebina)


def get_dict_from_ad_block(seznam):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    podatki_oglasov = []
    for oglas in oglasi:
        vzorec = re.compile(
            r'<h3><a title="(?P<ime>.*?)"'
            r'.*?'
            r'</a></h3>\n\n\s*(?P<opis>.*?)\s*<div class="additionalInfo">'
            r'.*?'
            r'<div class="price"><?s?p?a?n?>?(?P<cena>.*?\€?)\s*<?/?s?p?a?n?>?</div>.*?',
            re.DOTALL
            )
        for ujemanje in vzorec.finditer(oglas):
            podatki_oglasa = ujemanje.groupdict()
            podatki_oglasov.append(podatki_oglasa)
    return podatki_oglasov

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(directory, filename):
    '''Parse the ads in filename/directory into a dictionary list.'''
    vsebina = read_file_to_string(directory, filename)
    oglasi = page_to_ads(vsebina)
    seznam_slovarjev = get_dict_from_ad_block(oglasi)
    return seznam_slovarjev

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.

cats = ads_from_file(cat_directory, frontpage_filename)


def write_cat_ads_to_csv(seznam_slovarjev):
    write_csv(['ime', 'opis', 'cena'], seznam_slovarjev, cat_directory, csv_filename)
    return None
