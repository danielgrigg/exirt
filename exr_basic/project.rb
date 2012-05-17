PACKAGE_OPENEXR = {
  :name => "OpenEXR",
#  :components => "",                                                           
  :required => true,
  :optional_cmake => ""  # Insert package-missing-handler
}

PROJECT = 
{
  :name => "exr_basic", 
  :cmake_version => "2.6",

  :targets => 
  [{
    :name => "exr_basic",
    :type => :shared, # (:static, :shared, :executable)
    :install => true, 
    :sources => "src", # Source root

    :common => 
    {
      :packages => [PACKAGE_OPENEXR], # list of package dictionaries
      :definitions => [], # string-list of extra compiler definitions
      :include_dirs => ["src"], # string-list of extra include dirs
      :link_dirs => [], # string-list of extra link-dirs
      :libs => [] # string-list of extra libs
    },
    # :apple, :linux and :windows platforms are optional 
    :apple => 
    {
      :packages => [], 
      :definitions => [], 
      :include_dirs => [],
      :link_dirs => [] 
    },
    :linux => 
    {
      :packages => [],
      :definitions => [],
      :include_dirs => [],
      :link_dirs => [] 
    }
  }, {
    :name => "helloworld",
    :type => :executable,
    :depends => "exr_basic",
    :install => false,
    :sources => "apps/helloworld",
    :common => {
    :packages => [],
    :definitions => [],
    :include_dirs => [],
    :link_dirs => [],
    :libs => ["exr_basic"]                                                          
  }}]
}

