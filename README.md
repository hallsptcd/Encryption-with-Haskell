# Language Encryption and Detection

**Language**: Haskell

**Introduction**: These are I/O Haskell functions designed for two purposes: (1) to encrypt and decrypt text files using a caesar cipher and (2) to detect the language of a text file by comparing character frequency distributions.

**Data Files**: The datasets include language.hs, the primary file containing the functions. There is also encrypt.hs, used to encrypt files from the command line, decrypt.hs to decrypt, and guess_index.hs to determine the encryption index of an unknown encrypted file. This is done by building a dictionary of English words from excerpts from three novels: Ulysses, Pride and Prejudice, and The Picture of Dorian Gray. A file is considered decrypted when the cipher produces text with known English words. In addition, there is get_lang.hs which will tell you whether a language is likely written in English or in Portuguese based on chartacter frequency distribution.


