# Webby - The declarative Webserver

Webby is a Webserver, entirely written in Haskell. Which makes it cool, because it works functional!

### Get your Documents!
Well, its a Webserver. That's what it does...

### Configure it!
Via a handy configuration file, Webby satifies all your configurational needs (as long it's setting the port or the root directory)

### Look at that speed!!!
Think of all the time you gain, compared to other, professional webservers. No hour-long configuration needed. Build, run, have fun! Read a book or enjoy the sunset (while waiting for your response ;-) )

### Build
`stack build`

### Run
`stack exec Webserver-exe`

### Configure
Edit the file `webby.cfg` in the repository root

For more information, visit our [documentation site][docu]

**Webby | functional elegance **

##### Who did what?
Florian Pirchmoser:
- Response building and sending
- Different response types and statuscodes
- Reading from filesystem
- Passing configuration to affected functions
- Errorhandling (including basic structure for that)
- Testing and documenting own code

Johannes Brühl:
- First structure
- Build
- Request parsing
- Configurating the port and context root
- Errorhandling (using basic structure from Flo)
- Testing and documenting own code


   [docu]: <https://ob-fun-ws17.github.io/studienarbeit-flojo/index.html>
