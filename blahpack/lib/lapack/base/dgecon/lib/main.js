

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dgecon = require( './dgecon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgecon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgecon;
