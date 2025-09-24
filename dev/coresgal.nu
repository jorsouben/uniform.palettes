open `Bandera Galicia.svg` --raw | parse --regex '(?<=fill:)(?P<color>#[0-9a-fA-F]{6})' | uniq
