
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zggsvp3 = require( './zggsvp3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zggsvp3, 'ndarray', ndarray );


// EXPORTS //

module.exports = zggsvp3;
