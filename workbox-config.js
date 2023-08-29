module.exports = {
	globDirectory: 'dist/',
	globPatterns: [
		'**/*.{png,js,html}'
	],
	swDest: 'dist/sw.js',
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	]
};