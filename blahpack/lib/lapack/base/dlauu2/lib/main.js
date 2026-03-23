

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlauu2 = require( './dlauu2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlauu2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlauu2;
