# On library attachment, print message to user.
.onAttach <- function(libname, pkgname) {

    msg <- paste0(
        "Welcome to healthyR! ------------------------------------------------",
        "\nIf you find this package useful, please leave a star: https://github.com/spsanderson/healthyR'",
        "\nIf you encounter a bug or want to request an enhancement please file an issue at:",
        "\n   https://github.com/spsanderson/healthyR/issues",
        "\nThank you for using healthyR!"
    )

    packageStartupMessage(msg)

}
