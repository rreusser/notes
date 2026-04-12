
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zheswapr = require( './zheswapr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zheswapr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zheswapr;
