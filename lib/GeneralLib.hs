module GeneralLib where

class PrettyPrintable t where
  prettyPrint :: t -> String