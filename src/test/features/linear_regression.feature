Feature: Linear Regression
  In order to learn the characteristics of a linear model
  As an engineer
  I want to be able to run a linear regression on a data set

  Scenario: Execute Linear Regression on the simple housing data
    Given the simple housing data
    When I apply the linear regression
    Then I should get the right theta for the simple housing data result
    
  Scenario: Execute Linear Regression on the complex housing data
    Given the complex housing data
    When I apply the linear regression
    Then I should get the right theta for the complex housing data result
