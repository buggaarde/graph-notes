Feature: Add new tag
  In order to add a new tag
  As a graph-notes user
  I want to execute `graph-notes--create-new-tag'

  Scenario: Create new tag-file
    Given A file-extension ".md"
    When I execute "graph-notes--create-new-tag" with arg "test"
    Then The file "test.md" should exist

  Scenario: Create a tag with default file-extension
    When I execute "graph-notes--create-new-tag" with arg "test"
    Then The file "test.org" should exist

  Scenario: List all tags
    Given A tag "test"
    When I execute "graph-notes--list-all-tags"
    Then The tag "test" should be in the tags-list

  Scenario: Remove a tag
    Given A file-extension ".md"
    And A tag "test"
    When I execute "graph-notes--remove-tag" with arg "test"
    Then The tag "test" should not be in the tags-list
    And The file "test.md" should not exist
