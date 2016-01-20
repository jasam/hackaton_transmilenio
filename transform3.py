import os
import timeit
import string
import pandas as pd

def get_filepaths(directory):
    """
    This function will generate the file names in a directory 
    tree by walking the tree either top-down or bottom-up. For each 
    directory in the tree rooted at directory top (including top itself), 
    it yields a 3-tuple (dirpath, dirnames, filenames).
    """
    file_paths = []  # List which will store all of the full filepaths.

    # Walk the tree.
    for root, directories, files in os.walk(directory):
        for filename in files:
            # Join the two strings in order to form the full filepath.
            filepath = os.path.join(root, filename)
            file_paths.append(filepath)  # Add it to the list.

    return file_paths  # Self-explanatory.

# Run the above function and store its results in a variable.
directory = "F:/data/transmilenio/Datos/DATOS TRANSMILENIO/F1 Y F2 2013/MAYO/"   
full_file_paths = get_filepaths(directory)

start_time = timeit.default_timer()

for f in full_file_paths:
  if f.endswith(".xlsx"):
    # Convert file
    data_xls = pd.read_excel(f, sheetname=0)
    new_f = f.replace('xlsx', 'csv')
    data_xls.to_csv(new_f, encoding='utf-8')
    print(new_f)

elapsed = timeit.default_timer() - start_time
print("time elapsed" + str(elapsed))  