
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dggev = require( './dggev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dggev, 'ndarray', ndarray );


// EXPORTS //

module.exports = dggev;
