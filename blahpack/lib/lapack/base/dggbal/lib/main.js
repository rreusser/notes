
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dggbal = require( './dggbal.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dggbal, 'ndarray', ndarray );


// EXPORTS //

module.exports = dggbal;
