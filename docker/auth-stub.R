# Bypass dwhr::authenticate() for the demo container.
#
# The default authenticate() in dwhr enforces session$userData$authenticated
# and routes through ShinyProxy headers / AD lookup when omgeving != 'NONE'.
# In the container we want zero auth ceremony — anyone reaching :4815 is
# considered the demo user. Production deployments should NEVER use this
# stub; they need the real auth flow described in docs/DEPLOYMENT.md.
#
# Sourced from /entrypoint.R before the example's server.R runs.

local({
    if (!"dwhr" %in% loadedNamespaces()) return(invisible())

    stub <- function(session, ...) {
        session$userData$authenticated <- TRUE
        session$userData$dashUser      <- "demo"
        session$userData$dashUserName  <- "Demo User"
        session$userData$dashUserFunc  <- "demo"
        invisible(TRUE)
    }

    suppressWarnings(
        utils::assignInNamespace("authenticate", stub, ns = "dwhr")
    )

    cat("[auth-stub] dwhr::authenticate replaced with bypass stub.\n")
})
