There is only one main program file.

Step 1. Load the main file
Command: (load "slither.lsp")
If the directory in which zip folder is different, then that pathname needs to be specified

Step 2. After loading the file, call the main function slither
Command: (slither)

Step 3. Game Instructions are first demonstrated. Game will prompt you if you want to play slither or nothing
Option1: yes
Command: y
Use, this command if you wish to play slither, It will take you to Step 4.
Option 2: no
Command : n
Use, this command if you do not wish to play slither, It will lead you out the game.

Step 4. Program will prompt you if you want to load a file automatically
Option: yes
Command: y 
Use, this command if you want to load default file "testboard.txt" which is placed in the boards folder

Option:no
Command: n
Use, this command if you want to input a file name to be loaded.All the boards are placed in the folder
under Boards folder in zip file submitted with code.txt

Step 5. Only applicable if option 2 is selected in Step 4
Command: Board4.txt
Enter plain text, nothing else is required with use of any extra symbols
You can load any of the files by giving the path of file. Where ever, the zip folder is saved, inside the Boards folder there are all the encoded boards 
and that path needs to be specified at the prompt without any extra symbols.
eg: C:/Desktop/Boards/Board2.txt

Step 6. Desired file is loaded, now the program will prompt you if you want to play manually or want computer to solve automatically.
Option 1: yes
Command: y
Use, this command if you want to play manually by entering moves

Option 2: no
Command : n
Use, this command if you want computer to solve automatically. This will give you time taken to solve that problem

Step 7. Only applicable if option 1 was selected in Step 6
Enter moves in format of "1 2 t" without the symbols, if your solution is correct, game will prompt "You win" along with the list of moves.

Step 8. Only applicable if option 2 was selected in Step 6
Initial board is shown, Press and key and then press enter it will show you the solution and time taken for it.





 