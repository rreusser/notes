
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarz = require( './dlarz.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarz, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarz;
