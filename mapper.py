# Run on windows 10 x64, python 3.7.2

import bioc
import csv
import io
import operator
import os
import pandas
import re

from nltk import word_tokenize
from string import punctuation


# taken from https://github.com/yfpeng/bioc
def parse_to_string(filename):
    with open(filename, 'r') as fp:
        collection = bioc.load(fp)

    return collection

def get_document(collection, name):
    ret = None
    for document in collection.documents:
        if document.id == name:
            ret = document

    return(ret)


def join_submatches(zipped_raw):

    ret = []
    
    for i in reversed(range(0, len(zipped_raw))):
        found_j = None
        j = i - 1
        while j >= 0:
            if zipped_raw[i][0] >= zipped_raw[j][0] and zipped_raw[i][1] <= zipped_raw[j][1]:
                found_j = j
                break
            j = j - 1

        if found_j is not None:
            ret.append((i, found_j))
        else:
            ret.append((i, i))


    ret.reverse()
    return(ret)

def dict_to_csv(data, filename = None, prefix = None, colnames = None):
    assert(filename is not None)
    assert(prefix is not None)

    with open('./data/' + filename, 'w') as f:     
        if colnames is not None:
            f.write(colnames + '\n')
        
        ctr = 0
        for key in data.keys():

            code = prefix + str(ctr).zfill(6) 
            s = code + '|' + key + '|' 
            tags = list(data[key])
            for i in range(0, len(tags) - 1):
                s += tags[i] + ';'
            s += tags[-1] + '\n'
                
            f.write(s)
            ctr += 1


def csv_to_dict(filepath = None):
    assert(filepath is not None)
    rez = dict()

    df = pandas.read_csv(filepath, sep = '|', header = 0) 
    nrow = df.shape[0]

    for i in range(0, nrow):
        key = df.loc[i]['Food Concept'].strip()
        val = df.loc[i]['ID'].strip()

        if key in rez:
            raise Exception()
        rez[key] = val

    return(rez)

def foodbase_set(collections, filename = None):

    assert(filename is not None)
    rez = dict()

    for collection in collections:
        for doc in collection.documents:

            for ann in doc.annotations:
                text = ann.text.strip().upper()
                tags = ann.infons['semantic_tags'].strip().split(sep = ';')[:-1]

                if text not in rez:
                    rez[text] = set()

                for t in tags:
                    rez[text].add(t.strip())

    dict_to_csv(rez, filename = filename, prefix = 'A', colnames="ID|Food Concept|Semantic Tags")

    return None
        
        
def onto_set(onto = None, filename = None):
    
    if filename is None:
        print("No filename given!")
        return(-1)
    if onto is None:
        print("No ontology selected!")
        return(-1)
    if onto not in ['OF', 'FOODON', 'SNOMEDCT', 'MESH', 'NDDF', 'RCD', 'SNMI']:
        print("No such ontology supported!")
        return(-1)

    prefix = None
    if onto is "FOODON":
        prefix = 'B'
    elif onto is "SNOMEDCT":
        prefix = 'C'
    elif onto is "OF":
        prefix = 'D'
    elif onto is "RCD":
        prefix = 'E'
    elif onto is "MESH":
        prefix = 'F'
    elif onto is "SNMI":
        prefix = 'G'
    elif onto is "NDDF":
        prefix = 'H'

    rez = dict()

    # Curated 

    path = './NCBO_annotations/csv/' + onto 
    files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f))]


    for f in files:

        full_f = path + '/' + f
        df = pandas.read_csv(full_f, sep = ',', index_col = 0, header = 0) 
        zipped_raw = list(zip(df['from'], df['to'], df['text'], df['urls']))
        zipped_raw.sort(key = lambda t: t[1] - t[0], reverse=True)

        #i = 0
        #for tup in zipped_raw:
            #print(i, tup[:-1], sep = ' ')
            #i += 1
        matches = join_submatches(zipped_raw)

        for inner, outer in matches:
            
            if(inner < outer):
                raise Exception("Sup/Sub mismatch!")

            text_outer = zipped_raw[outer][2].strip().upper()
            url_outer = zipped_raw[outer][3].strip()
            url_inner = zipped_raw[inner][3].strip()

            if text_outer not in rez:
                rez[text_outer] = set()
            
            rez[text_outer].add(url_outer)
            rez[text_outer].add(url_inner)

    print("Curated done.")

    # Uncurated

    path = './NCBO_annotations/csv_uncur/' + onto 
    files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f))]
    
    ctr = 0
    for f in files:

        if ctr > 1000000:
            break
        if ctr % 1000 == 0:
            print(ctr)

        full_f = path + '/' + f
        df = pandas.read_csv(full_f, sep = ',', index_col = 0, header = 0) 
        zipped_raw = list(zip(df['from'], df['to'], df['text'], df['urls']))
        zipped_raw.sort(key = lambda t: t[1] - t[0], reverse=True)

        #i = 0
        #for tup in zipped_raw:
            #print(i, tup[:-1], sep = ' ')
            #i += 1
        matches = join_submatches(zipped_raw)

        for inner, outer in matches:
            
            if(inner < outer):
                raise Exception("Sup/Sub mismatch!")

            text_outer = zipped_raw[outer][2].strip().upper()
            url_outer = zipped_raw[outer][3].strip()
            url_inner = zipped_raw[inner][3].strip()

            if text_outer not in rez:
                rez[text_outer] = set()
            
            rez[text_outer].add(url_outer)
            rez[text_outer].add(url_inner)

        ctr += 1

    print("Un-curated done.")

    #print(rez)
    dict_to_csv(rez, filename = filename, prefix = prefix, colnames="ID|Food Concept|URLs")

    
def match_concept(tokens, words, curr_idx):

    rez = None
    iter_count = 0
    for i in reversed(range(0, curr_idx+1)):
        if iter_count > 0:
            break
        iter_count += 1
        found = True
        j = i
        for word in words:
            #print(word.upper(), tokens[j].upper(), sep = ' ' )
            if word.upper() != tokens[j].upper():
                found = False
                break
            j += 1
            if j >= len(tokens):
                break

        if found:
            rez = i
            break

    return rez


def remove_duplicates(zipped_raw):

    zipped = []

    for i in reversed(range(0, len(zipped_raw))):
        keep = True
        j = i - 1
        while j >= 0:
            if zipped_raw[i][0] >= zipped_raw[j][0] and zipped_raw[i][1] <= zipped_raw[j][1]:
                keep = False
                break
            j = j - 1

        if keep:
            zipped.append(zipped_raw[i])

    zipped.reverse()
    return(zipped)

def char_to_token(zipped, text, tokens, recipe_id):
    zipped_converted = []
    char_count = 1
    concept_idx = 0
    for i in range(0, len(tokens)):
        
        if concept_idx >= len(zipped) or char_count >= len(text):
            break

        if zipped[concept_idx][0] <= char_count:
            #print(i, zipped[concept_idx][0], char_count, zipped[concept_idx][2], sep = ' | ')

            words = word_tokenize(zipped[concept_idx][2])
            where = match_concept(tokens, words, i)

            # False Positives from the ontologies produce None
            if where is not None:
                zipped_converted.append((where+1, zipped[concept_idx][1]-zipped[concept_idx][0] + 1,zipped[concept_idx][2]))
                
            concept_idx += 1

        char_count += len(tokens[i])
        #print("Char is: " + text[char_count-1] + " at " + str(char_count-1))
        #print("----")
        if char_count-1 >= len(text):
            with open("tocki.txt", "a") as f:
                f.write(recipe_id + '\n')

            break
        elif text[char_count-1] == ' ':
            char_count += 1

    return zipped_converted


def compare_spelling(word1, word2, threshold):

    n = min(len(word1), len(word2))

    ret = 0
    for i in range(0, n):
        if word1[i] is not word2[i]:
            break
        ret += 1


    return ret < threshold



def mapper(curated_collection, uncurated_collection, verbose = False):
    assert(curated_collection is not None)
    assert(uncurated_collection is not None)

    # final mapping
    rez_mapping = dict()

    # map: concept -> ID
    A_dict = csv_to_dict("./data/sets/csv/A_set_all.csv")
    B_dict = csv_to_dict("./data/sets/csv/B_set_all.csv")
    C_dict = csv_to_dict("./data/sets/csv/C_set_all.csv")
    D_dict = csv_to_dict("./data/sets/csv/D_set_all.csv")
    E_dict = csv_to_dict("./data/sets/csv/E_set_all.csv")
    F_dict = csv_to_dict("./data/sets/csv/F_set_all.csv")
    G_dict = csv_to_dict("./data/sets/csv/G_set_all.csv")
    H_dict = csv_to_dict("./data/sets/csv/H_set_all.csv")


    for onto in ["FOODON", "SNOMEDCT", "OF", 'MESH', 'NDDF', 'RCD', 'SNMI']:

        prefix = None
        onto_dict = None
        if onto is "FOODON":
            prefix = 'B'
            onto_dict = B_dict
        elif onto is "SNOMEDCT":
            prefix = 'C'
            onto_dict = C_dict
        elif onto is "OF":
            prefix = 'D'
            onto_dict = D_dict
        elif onto is "RCD":
            prefix = 'E'
            onto_dict = E_dict
        elif onto is "MESH":
            prefix = 'F'
            onto_dict = F_dict
        elif onto is "SNMI":
            prefix = 'G'
            onto_dict = G_dict
        elif onto is "NDDF":
            prefix = 'H'
            onto_dict = H_dict
        
        print(onto)
        print("Starting curated.")
        # Curated
        path = './NCBO_annotations/csv/' + onto 
        files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f))]
        
        ctr = 0

        for f in files:

            if ctr > 10000000:
               break

            recipe_id = f.split('_')[0]
            full_f = path + '/' + f

            # ------ Reading and formatting FoodBase data ------ #
            document = get_document(curated_collection, recipe_id)
            text = document.infons['full_text'].strip()
            tokens = word_tokenize(text)
                
            t_loc = []
            t_len = []
            t_text = []
            for annotation in document.annotations:
                t_loc.append(annotation.locations[0].offset)
                t_len.append(annotation.locations[0].length)
                t_text.append(annotation.text.upper())

            ground_truth = list(zip(t_loc, t_len, t_text))

            
            # ------ Reading and formatting onto data ------ #
            df = pandas.read_csv(full_f, sep = ',', index_col = 0, header = 0) 
            zipped_raw = list(zip(df['from'], df['to'], df['text']))
            zipped_raw.sort(key = lambda t: t[1] - t[0], reverse=True)

            zipped = remove_duplicates(zipped_raw)
            zipped.sort(key=lambda x: x[0])
            zipped_converted = char_to_token(zipped, text, tokens, recipe_id)

            #i = 0
            #for tup in zipped:
                #print(i, tup, sep = ' ')
                #i += 1

            if verbose:
                print(zipped_converted)
                print(ground_truth)

            # ------ Sanity check ------ #
            for p in ground_truth:
               i = p[0] - 1
                #for toks in p[2].split(sep=' '):

               for toks in word_tokenize(p[2]):
            #       print(toks, tokens[i])
                   assert(toks.upper() == tokens[i].upper())
                   i = i + 1    
            
            # ------ Mapping ------ #

            for z in zipped_converted:            

                for fidx in range(0, len(ground_truth)):
                    g = ground_truth[fidx]

                    if (z[0] == g[0] and z[1] == g[1]) or (z[0] == g[0] and z[1] < g[1]) or (z[0] > g[0] and z[0] < (g[0] + len(word_tokenize(g[2])))):
                            
                        g_key = A_dict[g[2].strip()].strip()
                        z_val = onto_dict[z[2].strip()].strip()

                        if g_key not in rez_mapping:
                            rez_mapping[g_key] = dict()
                            rez_mapping[g_key]['B'] = set()
                            rez_mapping[g_key]['C'] = set()
                            rez_mapping[g_key]['D'] = set()
                            rez_mapping[g_key]['E'] = set()
                            rez_mapping[g_key]['F'] = set()
                            rez_mapping[g_key]['G'] = set()
                            rez_mapping[g_key]['H'] = set()

                        rez_mapping[g_key][prefix].add(z_val)
                        #print("Mapped ", z[2], " to ", g[2])
                        #print("With ", g_key, " to ", z_val)

                    break

            ctr += 1
        print(onto + " curated done.\n")

        print("Starting un-curated.")

        # Uncurated
        path = './NCBO_annotations/csv_uncur/' + onto 
        #path = './csv_testing/' + onto 
        
        files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f))]
        
        ctr = 0

        for f in files:

            if ctr > 10000000:
               break

            if ctr % 1000 == 0:
               print(ctr)

            ecip = f.split('_')[0]
            recipe_id = ecip[0] + 'r' + ecip[1:]
            full_f = path + '/' + f

            # ------ Reading and formatting FoodBase data ------ #
            document = get_document(uncurated_collection, recipe_id)
            if document is None:
                continue
            text = document.infons['full_text'].strip()
            tokens = word_tokenize(text)
                
            t_loc = []
            t_len = []
            t_text = []
            for annotation in document.annotations:
                t_loc.append(annotation.locations[0].offset)
                t_len.append(annotation.locations[0].length)
                t_text.append(annotation.text.upper())

            ground_truth = list(zip(t_loc, t_len, t_text))

            
            # ------ Reading and formatting onto data ------ #
            df = pandas.read_csv(full_f, sep = ',', index_col = 0, header = 0) 
            zipped_raw = list(zip(df['from'], df['to'], df['text']))
            zipped_raw.sort(key = lambda t: t[1] - t[0], reverse=True)

            zipped = remove_duplicates(zipped_raw)
            zipped.sort(key=lambda x: x[0])
            zipped_converted = char_to_token(zipped, text, tokens, recipe_id)

            #i = 0
            #for tup in zipped:
                #print(i, tup, sep = ' ')
                #i += 1

            if verbose:
                print(zipped_converted)
                print(ground_truth)

            # ------ Sanity check ------ #
            for p in ground_truth:
               i = p[0] - 1
               #for toks in p[2].split(sep=' '):
               for toks in word_tokenize(p[2]):
                    
                    if abs(len(toks) - len(tokens[i]) > 2):
                        print("###########WTF#############")
                        print(toks, ' ', tokens[i])
                        with open("todelete.txt", 'a') as fdelete:
                            fdelete.write(recipe_id + '\n' + toks + ' ' + tokens[i] + '\n\n')
                        c = input()
                    #print(toks, tokens[i], sep = ' || ')
                    #assert(toks.upper() == tokens[i].upper())
                    i = i + 1    
            
            # ------ Mapping ------ #

            for z in zipped_converted:            

                for fidx in range(0, len(ground_truth)):
                    g = ground_truth[fidx]

                    if (z[0] == g[0] and z[1] == g[1]) or (z[0] == g[0] and z[1] < g[1]) or (z[0] > g[0] and z[0] < (g[0] + len(word_tokenize(g[2])))):
                            
                        g_key = A_dict[g[2].strip()].strip()
                        z_val = onto_dict[z[2].strip()].strip()

                        if g_key not in rez_mapping:
                            rez_mapping[g_key] = dict()
                            rez_mapping[g_key]['B'] = set()
                            rez_mapping[g_key]['C'] = set()
                            rez_mapping[g_key]['D'] = set()
                            rez_mapping[g_key]['E'] = set()
                            rez_mapping[g_key]['F'] = set()
                            rez_mapping[g_key]['G'] = set()
                            rez_mapping[g_key]['H'] = set()

                        rez_mapping[g_key][prefix].add(z_val)
                        #print("Mapped ", z[2], " to ", g[2])
                        #print("With ", g_key, " to ", z_val)

                    break
            
            ctr += 1
        print(onto + " uncurated done.\n")

    return rez_mapping


        
def mapping_to_csv(data, fname):

    with open(fname, 'w') as f:
        f.write("FoodBase|FOODON|SNOMEDCT|OF|RCD|MESH|SNMI|NDDF\n")
        for key in data:

            B_kod = ""
            for bs in data[key]['B']:
                B_kod += (bs.strip() + ';')
            if not B_kod:
                B_kod = 'NULL'

            C_kod = ""
            for cs in data[key]['C']:
                C_kod += (cs.strip() + ';')
            if not C_kod:
                C_kod = 'NULL'

            D_kod = ""
            for ds in data[key]['D']:
                D_kod += (ds.strip() + ';')
            if not D_kod:
                D_kod = 'NULL'

            E_kod = ""
            for es in data[key]['E']:
                E_kod += (es.strip() + ';')
            if not E_kod:
                E_kod = 'NULL'

            F_kod = ""
            for fs in data[key]['F']:
                F_kod += (fs.strip() + ';')
            if not F_kod:
                F_kod = 'NULL'

            G_kod = ""
            for gs in data[key]['G']:
                G_kod += (gs.strip() + ';')
            if not G_kod:
                G_kod = 'NULL'

            H_kod = ""
            for hs in data[key]['H']:
                H_kod += (hs.strip() + ';')
            if not H_kod:
                H_kod = 'NULL'

            
            
            
            f.write(key.strip() + "|" + B_kod +  "|" +  C_kod + "|" + D_kod + "|" + E_kod + "|" + F_kod + "|" + G_kod + "|" + H_kod + "\n")




if __name__ == '__main__':
    
    # Creating the FoodBase data set. (A)
    # collections = []
    # collections.append(parse_to_string("FoodBase_curated.xml"))
    # collections.append(parse_to_string("FoodBase_uncurated.xml"))
    # foodbase_set(collections, 'A_set.csv')

    # Creating the ontologies' data sets. (B, C, D)
    # onto_set("FOODON", "B_set_all.csv")
    # onto_set("SNOMEDCT", "C_set_all.csv")
    # onto_set("OF", "D_set_all.csv")
    # onto_set("RCD", "E_set_all.csv")
    # onto_set("MESH", "F_set_all.csv")
    # onto_set("SNMI", "G_set_all.csv")
    # onto_set("NDDF", "H_set_all.csv")

    # Creating the mapping between A, B, C, D.
    ret = mapper(parse_to_string("FoodBase_curated.xml"), parse_to_string("FoodBase_uncurated.xml"), verbose=False)
    mapping_to_csv(ret, './data/maps/csv/map.csv')
    print("Mapping done.")