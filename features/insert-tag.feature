Feature: Insert tag
  In order to insert a new tag
  As a graph-notes user
  I want to execute `graph-notes-insert-tag'

  Scenario: Insert existing tag
    Given A file-extension ".md"
    And A tag "test"
    And I am in buffer "insert-tag.org"
    And I see the following text
    """
    Insert tag here: 
    """

    When I go to end of line
    And I execute "graph-notes-insert-tag" with arg "test"
    And I go to word "test"
    
    Then I should see the following text
    """
    Insert tag here: test
    """
    And Current point should have the button face 
    And The file "test.md" should exist
    
  Scenario: Insert non-existing tag
    Given A file-extension ".md"
    And I am in buffer "insert-tag.org"
    And The buffer is empty
    And I see the following text
    """
    Insert tag here: 
    """

    When I go to end of line
    And I execute "graph-notes-insert-tag" with arg "test"
    And I go to word "test"
    
    Then I should see the following text
    """
    Insert tag here: test
    """
    And Current point should have the button face 
    And The file "test.md" should exist
