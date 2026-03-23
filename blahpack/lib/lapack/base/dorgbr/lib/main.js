

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorgbr = require( './dorgbr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgbr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgbr;
