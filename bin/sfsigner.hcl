description = "S/MIME File Signer"
binaries    = ["sfsigner"]
test        = "sfsigner version"

version "0.0.0" {
  platform darwin amd64 {
    source = "https://github.com/syncom/sf-signer/releases/download/v0.0.0/sfsigner-darwin-x86_64"
    sha256 = "3e8692fa751983b75b11392968b6244e48b8b56a3363257286840ac35a696c06"
    on unpack {
      rename { from = "${root}/sfsigner-darwin-x86_64" to = "${root}/sfsigner" }
    }
  }

  platform linux amd64 {
    source = "https://github.com/syncom/sf-signer/releases/download/v0.0.0/sfsigner-linux-x86_64"
    sha256 = "7cfd5c00b8bc7215bc99972017da1837c514856db6d72b8e44e9136cb4fe9050"
    on unpack {
      rename { from = "${root}/sfsigner-linux-x86_64" to = "${root}/sfsigner" }
    }
  }
}
