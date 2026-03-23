

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zherk = require( './zherk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zherk, 'ndarray', ndarray );


// EXPORTS //

module.exports = zherk;
