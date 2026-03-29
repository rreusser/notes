'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var izmax1 = require( './izmax1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( izmax1, 'ndarray', ndarray );


// EXPORTS //

module.exports = izmax1;
