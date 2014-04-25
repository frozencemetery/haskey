module Prompts where

data Prompt = Prompt
  { message :: String
  , alert :: String
  }

unlockPrompt, tryAgainPrompt, newPasswordPrompt, confirmPasswordPrompt
 :: Prompt

unlockPrompt = Prompt
  { message = "Unlock keychain"
  , alert = "Please enter the password used to encrypt the keychain."
  }

tryAgainPrompt = Prompt
  { message = "Unlock keychain"
  , alert = "Incorrect password. Please try again."
  }

newPasswordPrompt = Prompt
  { message = "New password"
  , alert = "Please enter the a new password to encrypt the keychain."
  }

confirmPasswordPrompt = Prompt
  { message = "Confirm password"
  , alert = "Please re-enter the new password."
  }

focusFail, focusRetry, selectUser, selectService :: String
focusFail = "Couldn't grab focus; proceeding anyway!"
focusRetry = "Retrying focus grab..."
selectUser = "Please select a username from the above:"
selectService = "Please select a service from the above:"
