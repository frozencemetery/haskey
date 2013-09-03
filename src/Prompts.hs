module Prompts where

data Prompt = Prompt
  { message :: String
  , alert :: String
  }

unlockPrompt :: Prompt
unlockPrompt = Prompt
  { message = "Unlock keychain"
  , alert = "Please enter the password used to encrypt the keychain."
  }

tryAgainPrompt :: Prompt
tryAgainPrompt = Prompt
  { message = "Unlock keychain"
  , alert = "Incorrect password. Please try again."
  }

newPasswordPrompt :: Prompt
newPasswordPrompt = Prompt
  { message = "New password"
  , alert = "Please enter the a new password to encrypt the keychain."
  }

confirmPasswordPrompt :: Prompt
confirmPasswordPrompt = Prompt
  { message = "Confirm password"
  , alert = "Please re-enter the new password."
  }
