DATA_DIR = data
TAR_FILE = $(DATA_DIR)/enron1.tar.gz
BUILD_DIR = dist-newstyle
EXTRACTED_FILE = $(DATA_DIR)/enron1

GREEN = \\033[0;32m
YELLOW = \\033[1;33m
RESET = \\033[0m

.PHONY: dirs setup clean

run:
	@printf "$(YELLOW)In development :0$(RESET)\n"

clean:
	rm -rf $(DATA_DIR)
	rm -rf $(BUILD_DIR)

dirs:
	mkdir -p $(DATA_DIR)

$(EXTRACTED_FILE):
	wget http://nlp.cs.aueb.gr/software_and_datasets/Enron-Spam/preprocessed/enron1.tar.gz -O $(TAR_FILE)
	tar -xvzf $(TAR_FILE) -C $(DATA_DIR)
	rm -rf $(TAR_FILE)

extract: $(EXTRACTED_FILE)

frequency:
	cabal build frequency-bin
	cabal run frequency-bin

setup: dirs extract frequency
	@printf "$(GREEN)1. The bag of words has been successfully created for both HAM (data/ham-frequency.txt) and SPAM (data/spam-frequency.txt).$(RESET)\n"
	@printf "$(GREEN)2. Now execute \`make run\` to check whether a text is spam or not.$(RESET)\n"
