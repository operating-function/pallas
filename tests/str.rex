~~
    'String'
~~
    r'Name+String'
    r"Name+String"
~~
    r'What''s up?'
    r"She said: ""yooooo"""
~~
~~
    """ Line
~~
    $"f"
~~
    $ """ Name+Line
~~
    ''' Multiple
    ''' Lines
~~
    $ """ Name+
      """ Multiple
      """ Lines
~~
    """ Okay """
    """ Frey """
~~
    | """
~~
    | concat
        """ foo
      """ bar
    | concat
          $ """ foo
        $ """ bar
    | concat
          $ """ foo
        $ """ bar
          """ bar
    | concat
          $ """ foo
            """ foo
        $ """ bar
          """ bar
