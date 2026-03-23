

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dpocon = require( './dpocon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpocon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpocon;
