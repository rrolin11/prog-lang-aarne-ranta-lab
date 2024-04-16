int main()
{
	// generate the sentence
	vector<string> sentence = gen_sentence(read_grammar(cin));

	// write the first word, if any
#ifdef _MSC_VER
	std::vector<string>::const_iterator it = sentence.begin();
#else
	vector<string>::const_iterator it = sentence.begin();
#endif
	if (!sentence.empty()) {
		cout << *it;
		++it;
	}

	// write the rest of the words, each preceded by a space
	while (it != sentence.end()) {
		cout << " " << *it;
		++it;
	}

	cout << endl;
	return 0;
}