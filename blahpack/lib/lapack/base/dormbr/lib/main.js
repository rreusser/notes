

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dormbr = require( './dormbr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dormbr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dormbr;
