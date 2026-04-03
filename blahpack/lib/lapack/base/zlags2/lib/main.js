
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlags2 = require( './zlags2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlags2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlags2;
