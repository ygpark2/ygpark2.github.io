# The DocPad Configuration File
# It is simply a CoffeeScript Object which is parsed by CSON
docpadConfig = {

  # =================================
  # Collections
  # These are special collections that our website makes available to us

  collections:
    frontpage: ->
      @getFilesAtPath(['posts', 'pages', 'projects', 'pictures'], [{date: -1}]).findAllLive({layout: $ne: 'default'})
    posts: ->
      @getFilesAtPath('posts', [{date: -1}]).findAllLive({layout: $ne: 'default'}).on "add", (model) ->
        model.setMetaDefaults({layout: "post"})
    pages: ->
      @getFilesAtPath('pages', [{date: -1}]).findAllLive({layout: $ne: 'default'}).on "add", (model) ->
        model.setMetaDefaults({layout: "post"})
    projects: ->
      @getFilesAtPath('projects', [{date: -1}]).findAllLive({layout: $ne: 'default'}).on "add", (model) ->
        model.setMetaDefaults({layout: "post"})
    pictures: ->
      @getFilesAtPath('pictures', [{date: -1}]).findAllLive({layout: $ne: 'default'}).on "add", (model) ->
        model.setMetaDefaults({layout: "post"})


  # Regenerate Every
  # Performs a regenerate every x milliseconds, useful for always having the latest data
  regenerateEvery: false  # default


  # =================================
  # Template Configuration
  #

	# =================================
	# Template Data
	# These are variables that will be accessible via our templates
	# To access one of these within our templates, refer to the FAQ: https://github.com/bevry/docpad/wiki/FAQ

  templateData:

    # Specify some site properties
    site:
      # The production url of our website
      url: "http://website.com"

      # Here are some old site urls that you would like to redirect from
      oldUrls: [
        'www.website.com',
        'website.herokuapp.com'
      ]

      # The default title of our website
      title: "YGP Blog"

      # The website author's name
      author: "Your Name"

      # The website author's email
      email: "b@lupton.cc"

      # The website heading to be displayed on the page
      heading: "Your Website"

      # The website subheading to be displayed on the page
      subheading: """
        Welcome to your new <t>links.docpad</t> website!
        """

      # Footer
      footnote: """
        This website was created with <t>links.bevry</t>��s <t>links.docpad</t>
        """

      copyright: """
        Your chosen license should go here...
        Not sure what a license is? Refer to the
        <code>README.md</code> file included in this website.
        """

      # The website author's name
      # author: "Young Gyu Park"

      # The website author's email
      # email: "ygpark2@gmail.com"

   		# cache-busting timestamp
   		# timestamp: new Date().getTime()

			# The website description (for SEO)
      description: """
        When your website appears in search results in say Google, the text here will be shown underneath your website's title.
        """

			# The website keywords (for SEO) separated by commas
      keywords: """
        place, your, website, keywoards, here, keep, them, related, to, the, content, of, your, website
        """

      # The website's styles
      styles: [
        '/vendor/normalize.css'
        '/vendor/h5bp.css'
        'http://yandex.st/highlightjs/8.0/styles/zenburn.min.css'
        '/styles/bootstrap/bootstrap.css'
        '/styles/style.css'
      ]

      # The website's scripts
      scripts: [
        '/vendor/log.js'
        '/vendor/modernizr.js'
        'http://yandex.st/highlightjs/8.0/highlight.min.js'
        '/scripts/bootstrap.js'
        '/scripts/script.js'
      ]

      feeds: [
        # This is the feed generated by our DocPad website
        # It contains all the posts, you can find the source file in src/documents/atom.xml.eco
        href: "/atom.xml"
        name: 'Blog Posts'
      ]

    # -----------------------------
    # Helper Functions

    # Get the prepared site/document title
    # Often we would like to specify particular formatting to our page's title
    # we can apply that formatting here
    getPreparedTitle: ->
      # if we have a document title, then we should use that and suffix the site's title onto it
      if @document.title
        "#{@document.title} | #{@site.title}"
      # if our document does not have it's own title, then we should just use the site's title
      else
        @site.title

    # Get the prepared site/document description
    getPreparedDescription: ->
      # if we have a document description, then we should use that, otherwise use the site's description
      @document.description or @site.description

    # Get the prepared site/document keywords
    getPreparedKeywords: ->
      # Merge the document keywords with the site keywords
      @site.keywords.concat(@document.keywords or []).join(', ')



  # =================================
  # DocPad Plugins
  #
  plugins:
    # Tags
    tags:
      extension: '.html.eco'
      injectDocumentHelper: (document) ->
        document.setMeta(
          layout: 'default'
          data: """
            <%- @partial('content/tag', @) %>
            """
        )
    ghpages:
      deployRemote: 'origin'
      deployBranch: 'master'
    rss:
      collection: 'posts'
      url: '/feed.xml' # optional, this is the default

  # =================================
  # DocPad Events

  # Here we can define handlers for events that DocPad fires
  # You can find a full listing of events on the DocPad Wiki
  events:

    # Server Extend
    # Used to add our own custom routes to the server before the docpad routes are added
    serverExtend: (opts) ->
      # Extract the server from the options
      {server} = opts
      docpad = @docpad

      # As we are now running in an event,
      # ensure we are using the latest copy of the docpad configuraiton
      # and fetch our urls from it
      latestConfig = docpad.getConfig()
      oldUrls = latestConfig.templateData.site.oldUrls or []
      newUrl = latestConfig.templateData.site.url

      # Redirect any requests accessing one of our sites oldUrls to the new site url
      server.use (req,res,next) ->
        if req.headers.host in oldUrls
          res.redirect(newUrl+req.url, 301)
        else
          next()
}

# Export our DocPad Configuration
module.exports = docpadConfig
