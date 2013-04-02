require "optparse"

module Evemacs
  class Options
    attr_reader :token ,:message, :notebook

    def initialize(argv)
      @token = nil
      @message = nil
      @notebook = nil

      parser = OptionParser.new

      parser.on("-t", "--taken=TOKEN", "evenote token") do |token|
        @token = token
      end

      parser.on("-m", "--message=MESSAGE", "added message") do |message|
        @message = message.force_encoding("UTF-8")
      end

      parser.on("-n", "--notebook=NOTEBOOK",
                "notebook to add message") do |notebook|
        @notebook = notebook.force_encoding("UTF-8")
      end

      parser.parse!(argv)
    end
  end
end
